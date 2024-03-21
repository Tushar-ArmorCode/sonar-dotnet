﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2024 SonarSource SA
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

using System.Collections.Concurrent;

namespace SonarAnalyzer.Rules.CSharp;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class SpecifyRouteAttribute : SonarDiagnosticAnalyzer
{
    private const string DiagnosticId = "S6934";
    private const string MessageFormat = "Specify the RouteAttribute when an HttpMethodAttribute is specified at an action level.";

    private static readonly DiagnosticDescriptor Rule = DescriptorFactory.Create(DiagnosticId, MessageFormat);

    private static readonly ImmutableArray<KnownType> RouteTemplateAttributes =
        ImmutableArray.Create(KnownType.Microsoft_AspNetCore_Mvc_Routing_HttpMethodAttribute, KnownType.Microsoft_AspNetCore_Mvc_RouteAttribute);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

    protected override void Initialize(SonarAnalysisContext context) =>
        context.RegisterCompilationStartAction(compilationStart =>
            {
                if (compilationStart.Compilation.GetTypeByMetadataName(KnownType.Microsoft_AspNetCore_Mvc_Routing_HttpMethodAttribute) is null)
                {
                    return;
                }
                compilationStart.RegisterSymbolStartAction(symbolStart =>
                {
                    if (symbolStart.Symbol.GetAttributes().Any(x => x.AttributeClass.Is(KnownType.Microsoft_AspNetCore_Mvc_RouteAttribute)))
                    {
                        return;
                    }
                    var secondaryLocations = new ConcurrentStack<Location>();
                    symbolStart.RegisterSyntaxNodeAction(nodeContext =>
                    {
                        var node = (MethodDeclarationSyntax)nodeContext.Node;
                        if (nodeContext.SemanticModel.GetDeclaredSymbol(node, nodeContext.Cancel) is { } method
                            && method.IsControllerMethod()
                            && method.GetAttributes().Any(x =>
                                x.AttributeClass.DerivesFromAny(RouteTemplateAttributes)
                                && x.TryGetAttributeValue<string>("template", out var template)
                                && !string.IsNullOrWhiteSpace(template)))
                        {
                            secondaryLocations.Push(node.Identifier.GetLocation());
                        }
                    }, SyntaxKind.MethodDeclaration);
                    symbolStart.RegisterSymbolEndAction(symbolEnd => ReportIssues(symbolEnd, symbolStart.Symbol, secondaryLocations));
                }, SymbolKind.NamedType);
            });

    private static void ReportIssues(SonarSymbolReportingContext context, ISymbol symbol, ConcurrentStack<Location> secondaryLocations)
    {
        if (!secondaryLocations.IsEmpty)
        {
            foreach (var declaration in symbol.DeclaringSyntaxReferences.Select(x => x.GetSyntax()))
            {
                if (declaration.GetIdentifier() is { } identifier)
                {
                    context.ReportIssue(CSharpGeneratedCodeRecognizer.Instance, Diagnostic.Create(Rule, identifier.GetLocation(), secondaryLocations));
                }
            }
        }
    }
}
