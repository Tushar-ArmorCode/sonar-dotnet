/*
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
using static SonarAnalyzer.Helpers.ArgumentDescriptor;
using static SonarAnalyzer.Helpers.KnownType;

namespace SonarAnalyzer.Rules.CSharp;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class UseModelBinding : SonarDiagnosticAnalyzer<SyntaxKind>
{
    private const string DiagnosticId = "S6932";
    private const string UseModelBindingMessage = "Use model binding instead of accessing the raw request data";
    private const string UseIFormFileBindingMessage = "Use IFormFile or IFormFileCollection binding instead";

private static readonly ArgumentDescriptor[] ArgumentDescriptors =
    [
        ElementAccess(Microsoft_AspNetCore_Http_IFormCollection, "Form", _ => true, 0),
        MethodInvocation(Microsoft_AspNetCore_Http_IFormCollection, "TryGetValue", "key", 0),
        MethodInvocation(Microsoft_AspNetCore_Http_IFormCollection, "ContainsKey", "key", 0),
        ElementAccess(Microsoft_AspNetCore_Http_IHeaderDictionary, "Headers", IsGetterParameter, 0),
        MethodInvocation(
            x => IsIDictionaryStringStringValuesInvocation(x, "TryGetValue"),
            (name, comparison) => string.Equals(name, "TryGetValue", comparison),
            IsAccessedViaHeaderDictionary,
            x => string.Equals(x.Name, "key", StringComparison.Ordinal),
            (list, position) => list.Count == 2 && (position is 0 or null), RefKind.None),
        MethodInvocation(
            x => IsIDictionaryStringStringValuesInvocation(x, "ContainsKey"),
            (name, comparison) => string.Equals(name, "ContainsKey", comparison),
            IsAccessedViaHeaderDictionary,
            x => string.Equals(x.Name, "key", StringComparison.Ordinal),
            (list, _) => list.Count == 1, RefKind.None),
        ElementAccess(Microsoft_AspNetCore_Http_IQueryCollection, "Query", _ => true, 0),
        MethodInvocation(Microsoft_AspNetCore_Http_IQueryCollection, "TryGetValue", "key", 0),
        ElementAccess(Microsoft_AspNetCore_Routing_RouteValueDictionary, "RouteValues", IsGetterParameter, 0),
        MethodInvocation(Microsoft_AspNetCore_Routing_RouteValueDictionary, "TryGetValue", "key", 0),
    ];

    private static readonly MemberDescriptor[] PropertyAccessDescriptors =
        [
            new(Microsoft_AspNetCore_Http_IFormCollection, "Files"), // Request.Form.Files...
        ];

    protected override string MessageFormat => "{0}";

    protected override ILanguageFacade<SyntaxKind> Language => CSharpFacade.Instance;

    public UseModelBinding() : base(DiagnosticId) { }

    protected override void Initialize(SonarAnalysisContext context) =>
        context.RegisterCompilationStartAction(compilationStartContext =>
        {
            compilationStartContext.RegisterSymbolStartAction(symbolStartContext =>
            {
                // If the user overrides any action filters, model binding may not be working as expected.
                // Then we do not want to raise on expressions that originate from parameters.
                // See the OverridesController.Undecidable test cases for details.
                var hasOverrides = false;
                var candidates = new ConcurrentStack<ReportCandidate>(); // In SymbolEnd, we filter the candidates based on the overriding we learn on the go.
                if (symbolStartContext.Symbol is INamedTypeSymbol namedType && namedType.IsControllerType())
                {
                    symbolStartContext.RegisterCodeBlockStartAction<SyntaxKind>(codeBlockStart =>
                    {
                        if (IsOverridingFilterMethods(codeBlockStart.OwningSymbol))
                        {
                            hasOverrides = true;
                        }
                        else
                        {
                            RegisterCodeBlockActions(codeBlockStart, candidates);
                        }
                    });
                }
                symbolStartContext.RegisterSymbolEndAction(symbolEnd =>
                {
                    foreach (var candidate in candidates.Where(x => !hasOverrides || !x.OriginatesFromParameter))
                    {
                        symbolEnd.ReportIssue(Diagnostic.Create(Rule, candidate.Location, candidate.Message));
                    }
                });
            }, SymbolKind.NamedType);
        });

    private void RegisterCodeBlockActions(
        SonarCodeBlockStartAnalysisContext<SyntaxKind> codeBlockStart,
        ConcurrentStack<ReportCandidate> controllerCandidates)
    {
        // Within a single code block, access via constant and variable keys could be mixed.
        // We only want to raise, if all access were done via de-facto constants.
        var allConstantAccess = true;
        var codeBlockCandidates = new ConcurrentStack<ReportCandidate>();
        codeBlockStart.RegisterNodeAction(nodeContext =>
            {
                if (!allConstantAccess)
                {
                    return;
                }
                var argument = (ArgumentSyntax)nodeContext.Node;
                var context = new ArgumentContext(argument, nodeContext.SemanticModel);
                if (Array.Exists(ArgumentDescriptors, x => Language.Tracker.Argument.MatchArgument(x)(context)))
                {
                    allConstantAccess = Language.FindConstantValue(nodeContext.SemanticModel, argument.Expression) is string;
                    codeBlockCandidates.Push(new(UseModelBindingMessage, GetPrimaryLocation(argument), OriginatesFromParameter(nodeContext.SemanticModel, argument)));
                }
            }, SyntaxKind.Argument);
        codeBlockStart.RegisterNodeAction(nodeContext =>
            {
                var memberAccess = (MemberAccessExpressionSyntax)nodeContext.Node;
                var context = new PropertyAccessContext(memberAccess, nodeContext.SemanticModel, memberAccess.Name.Identifier.ValueText);
                if (Language.Tracker.PropertyAccess.MatchProperty(PropertyAccessDescriptors)(context))
                {
                    codeBlockCandidates.Push(new(UseIFormFileBindingMessage, memberAccess.GetLocation(), OriginatesFromParameter(nodeContext.SemanticModel, memberAccess)));
                }
            }, SyntaxKind.SimpleMemberAccessExpression);
        codeBlockStart.RegisterCodeBlockEndAction(codeBlockEnd =>
        {
            if (allConstantAccess)
            {
                controllerCandidates.PushRange([.. codeBlockCandidates]);
            }
        });
    }

    // Check that the "Headers" expression in the Headers.TryGetValue("id", out _) invocation is of type IHeaderDictionary
    private static bool IsAccessedViaHeaderDictionary(SemanticModel model, ILanguageFacade language, SyntaxNode invocation) =>
        invocation is InvocationExpressionSyntax { Expression: { } expression }
        && GetLeftOfDot(expression) is { } left
        && model.GetTypeInfo(left) is { Type: { } typeSymbol } && typeSymbol.Is(Microsoft_AspNetCore_Http_IHeaderDictionary);

    private static bool IsOverridingFilterMethods(ISymbol symbol) =>
        symbol is IMethodSymbol method
        && (method.GetOverriddenMember() ?? method).ExplicitOrImplicitInterfaceImplementations().Any(x => x is IMethodSymbol { ContainingType: { } container }
        && container.IsAny(
                Microsoft_AspNetCore_Mvc_Filters_IActionFilter,
                Microsoft_AspNetCore_Mvc_Filters_IAsyncActionFilter));

    private static bool OriginatesFromParameter(SemanticModel semanticModel, ArgumentSyntax argument) =>
        GetExpressionOfArgumentParent(argument) is { } parentExpression
        && OriginatesFromParameter(semanticModel, parentExpression);

    private static bool OriginatesFromParameter(SemanticModel semanticModel, ExpressionSyntax expression) =>
        MostLeftOfDottedChain(expression) is { } mostLeft
        && semanticModel.GetSymbolInfo(mostLeft).Symbol is IParameterSymbol;

    private static ExpressionSyntax GetLeftOfDot(ExpressionSyntax expression) =>
        expression switch
        {
            MemberAccessExpressionSyntax memberAccessExpression => memberAccessExpression.Expression,
            MemberBindingExpressionSyntax memberBindingExpression => memberBindingExpression.GetParentConditionalAccessExpression()?.Expression,
            _ => null,
        };

    private static ExpressionSyntax MostLeftOfDottedChain(ExpressionSyntax root)
    {
        var current = root.GetRootConditionalAccessExpression() ?? root;
        while (current.Kind() is SyntaxKind.SimpleMemberAccessExpression or SyntaxKind.ElementAccessExpression)
        {
            current = current switch
            {
                MemberAccessExpressionSyntax { Expression: { } left } => left,
                ElementAccessExpressionSyntax { Expression: { } left } => left,
                _ => throw new InvalidOperationException("Unreachable"),
            };
        }
        return current;
    }

    private static ExpressionSyntax GetExpressionOfArgumentParent(ArgumentSyntax argument) =>
        argument switch
        {
            { Parent: BracketedArgumentListSyntax { Parent: ElementBindingExpressionSyntax { Parent: ConditionalAccessExpressionSyntax { Expression: { } expression } } } } => expression,
            { Parent: BracketedArgumentListSyntax { Parent: ElementAccessExpressionSyntax { Expression: { } expression } } } => expression,
            { Parent: ArgumentListSyntax { Parent: InvocationExpressionSyntax { Expression: { } expression } } } => expression,
            _ => null,
        };

    private static Location GetPrimaryLocation(ArgumentSyntax argument) =>
        ((SyntaxNode)GetExpressionOfArgumentParent(argument) ?? argument).GetLocation();

    private static bool IsGetterParameter(IParameterSymbol parameter) =>
        parameter.ContainingSymbol is IMethodSymbol { MethodKind: MethodKind.PropertyGet };

    private static bool IsIDictionaryStringStringValuesInvocation(IMethodSymbol method, string name) =>
        method.Is(System_Collections_Generic_IDictionary_TKey_TValue, name)
            && method.ContainingType.TypeArguments is { Length: 2 } typeArguments
            && typeArguments[0].Is(System_String)
            && typeArguments[1].Is(Microsoft_Extensions_Primitives_StringValues);

    private readonly record struct ReportCandidate(string Message, Location Location, bool OriginatesFromParameter = false);
}
