﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2023 SonarSource SA
 * mailto: contact AT sonarsource DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

using SonarAnalyzer.SymbolicExecution.Roslyn;

namespace SonarAnalyzer.Rules.CSharp;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class DeclareParameterBeforeUsage : SonarDiagnosticAnalyzer
{
    private const string DiagnosticId = "S6801";
    private const string MessageFormat = "'{0}' parameter should be declared on component '{1}' before usage.";

    // https://learn.microsoft.com/en-us/dotnet/api/microsoft.aspnetcore.components.rendering.rendertreebuilder.addattribute?view=aspnetcore-7.0
    private const int MinAddAttributeParameters = 2;

    private static readonly DiagnosticDescriptor Rule = DescriptorFactory.Create(DiagnosticId, MessageFormat);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

    protected override void Initialize(SonarAnalysisContext context) =>
        context.RegisterCompilationStartAction(cs =>
        {
            if (cs.Compilation.GetTypeByMetadataName(KnownType.Microsoft_AspNetCore_Components_IComponent) is not null)
            {
                cs.RegisterNodeAction(CheckMethodDeclarationSyntax, SyntaxKind.MethodDeclaration);
            }
        });

    private static void CheckMethodDeclarationSyntax(SonarSyntaxNodeReportingContext context)
    {
        var method = (MethodDeclarationSyntax)context.Node;
        if (!IsBuildRenderTreeMethod(method))
        {
            return;
        }

        ITypeSymbol currentComponent = null;
        Dictionary<string, ComponentDescriptor> descriptors = new();
        foreach (var invocation in method.DescendantNodes().OfType<InvocationExpressionSyntax>())
        {
            var targetMethod = context.SemanticModel.GetOperation(invocation).AsInvocation().Value.TargetMethod;
            if (targetMethod.Name.Equals("OpenComponent")
                && targetMethod.TypeArguments is { Length: 1 } arguments
                && arguments[0].BaseType.Is(KnownType.Microsoft_AspNetCore_Components_ComponentBase))
            {
                currentComponent = targetMethod.TypeArguments[0];
            }
            else if (currentComponent != null
                && targetMethod.Name.Equals("AddAttribute")
                && targetMethod.Parameters is { Length: >= MinAddAttributeParameters } parameters
                && parameters[1].Type.Is(KnownType.System_String)
                && invocation.ArgumentList.Arguments[1].Expression.StringValue(context.SemanticModel) is { } parameterName
                && GetComponentDescriptor(currentComponent, descriptors) is var descriptor
                && !descriptor.HasMatchUnmatchedParameters
                && !descriptor.Parameters.Contains(parameterName))
            {
                var location = context.Tree.FilePath.EndsWith("razor.g.cs", StringComparison.OrdinalIgnoreCase)
                    ? null // The diagnostic is reported at the beginning of the file because the attribute location cannot be mapped back.
                    : invocation.GetLocation();
                context.ReportIssue(Diagnostic.Create(Rule, location, parameterName, currentComponent.Name));
            }
            else if (targetMethod.Name.Equals("CloseComponent"))
            {
                currentComponent = null;
            }
        }
    }

    private static bool IsBuildRenderTreeMethod(MethodDeclarationSyntax method) =>
        method.NameIs("BuildRenderTree")
        && method.ParameterList.Parameters.Count == 1
        && method.Modifiers.Any(SyntaxKind.OverrideKeyword);

    private static ComponentDescriptor GetComponentDescriptor(ITypeSymbol typeSymbol, Dictionary<string, ComponentDescriptor> descriptors)
    {
        if (!descriptors.TryGetValue(typeSymbol.ToDisplayString(), out var descriptor))
        {
            var componentDescriptor = GetComponentDescriptor(typeSymbol);
            descriptors.Add(typeSymbol.ToDisplayString(), componentDescriptor);
            return componentDescriptor;
        }
        return descriptor;
    }

    private static ComponentDescriptor GetComponentDescriptor(ITypeSymbol typeSymbol)
    {
        var componentDescriptor = new ComponentDescriptor();
        var currentSymbol = typeSymbol as INamedTypeSymbol;
        while (currentSymbol != null)
        {
            foreach (var member in currentSymbol.GetMembers())
            {
                if (member is IPropertySymbol property
                    && property.GetAttributes().Where(x => x.AttributeClass.Is(KnownType.Microsoft_AspNetCore_Components_ParameterAttribute)).ToList() is { Count: 1 } parameterAttributes
                    && componentDescriptor.Parameters.Add(member.Name)
                    && parameterAttributes[0].NamedArguments.Any(x => x.Key.Equals("CaptureUnmatchedValues") && x.Value.Value is true))
                {
                    componentDescriptor.HasMatchUnmatchedParameters = true;
                }
            }

            currentSymbol = currentSymbol.BaseType;
        }
        return componentDescriptor;
    }

    private sealed record ComponentDescriptor
    {
        public ISet<string> Parameters { get; set; } = new HashSet<string>(StringComparer.Ordinal);
        public bool HasMatchUnmatchedParameters { get; set; }
    }
}
