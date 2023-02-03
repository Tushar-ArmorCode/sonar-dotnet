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

namespace SonarAnalyzer.Rules.CSharp;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class UnusedStringBuilder : UnusedStringBuilderBase<SyntaxKind, VariableDeclaratorSyntax, InvocationExpressionSyntax, ReturnStatementSyntax>
{
    protected override ILanguageFacade<SyntaxKind> Language => CSharpFacade.Instance;

    protected override string GetVariableName(VariableDeclaratorSyntax declaration) => declaration.GetName();

    protected override bool NeedsToTrack(VariableDeclaratorSyntax declaration, SemanticModel semanticModel) =>
        declaration.Initializer is not null
        && declaration.Initializer.Value is { } expression
        && IsStringBuilderObjectCreation(expression, semanticModel);

    protected override IList<InvocationExpressionSyntax> GetInvocations(VariableDeclaratorSyntax declaration) =>
        declaration.IsTopLevel()
        ? GetTopLevelInvocations(declaration)
        : declaration.Parent.Parent.Parent.DescendantNodes().OfType<InvocationExpressionSyntax>().ToList();

    protected override IList<ReturnStatementSyntax> GetReturnStatements(VariableDeclaratorSyntax declaration) =>
        declaration.IsTopLevel()
        ? new()
        : declaration.Parent.Parent.Parent.DescendantNodes().OfType<ReturnStatementSyntax>().ToList();

    protected override bool IsStringBuilderAccessed(string variableName, IList<InvocationExpressionSyntax> invocations) =>
        invocations.Any(x => x.Expression is MemberAccessExpressionSyntax { } member
            && IsSameVariable(member.Expression, variableName)
            && StringBuilderAccessMethods.Contains(member.GetName()));

    protected override bool IsPassedToMethod(string variableName, IList<InvocationExpressionSyntax> invocations) =>
        invocations.Any(x => x.ArgumentList.Arguments.Any(y => IsSameVariable(y.Expression, variableName)));

    protected override bool IsReturned(string variableName, IList<ReturnStatementSyntax> returnStatements) =>
        returnStatements.Any(x => IsSameVariable(x.Expression, variableName));

    private static bool IsStringBuilderObjectCreation(ExpressionSyntax expression, SemanticModel semanticModel) =>
        (expression is ObjectCreationExpressionSyntax || ImplicitObjectCreationExpressionSyntaxWrapper.IsInstance(expression))
        && ObjectCreationFactory.Create(expression).IsKnownType(KnownType.System_Text_StringBuilder, semanticModel);

    private static bool IsSameVariable(ExpressionSyntax expression, string variableName) =>
        expression.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>().Any(p => p.NameIs(variableName));

    private static IList<InvocationExpressionSyntax> GetTopLevelInvocations(VariableDeclaratorSyntax declaration)
    {
        List<InvocationExpressionSyntax> list = new();
        foreach (var globalStatement in declaration.Parent.Parent.Parent.Parent.DescendantNodes().OfType<GlobalStatementSyntax>())
        {
            foreach (var invocation in globalStatement.DescendantNodes().OfType<InvocationExpressionSyntax>())
            {
                list.Add(invocation);
            }
        }
        return list;
    }
}
