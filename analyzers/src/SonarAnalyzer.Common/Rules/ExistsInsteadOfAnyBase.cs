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

namespace SonarAnalyzer.Rules;

public abstract class ExistsInsteadOfAnyBase<TSyntaxKind, TInvocationExpression> : SonarDiagnosticAnalyzer<TSyntaxKind>
    where TSyntaxKind : struct
    where TInvocationExpression : SyntaxNode
{
    private const string DiagnosticId = "S6605";

    protected override string MessageFormat => """Collection-specific "Exists" method should be used instead of the "Any" extension.""";

    protected abstract bool TryGetOperands(TInvocationExpression node, out SyntaxNode left, out SyntaxNode right);
    protected abstract bool IsValueEquality(TInvocationExpression node);
    protected abstract bool HasOneArgument(TInvocationExpression node);
    protected abstract SyntaxToken? GetIdentifier(TInvocationExpression invocation);

    protected ExistsInsteadOfAnyBase() : base(DiagnosticId) { }

    protected sealed override void Initialize(SonarAnalysisContext context) =>
        context.RegisterNodeAction(Language.GeneratedCodeRecognizer, c =>
        {
            var invocation = (TInvocationExpression)c.Node;

            if (Language.GetName(invocation).Equals(nameof(Enumerable.Any), Language.NameComparison)
                && HasOneArgument(invocation)
                && TryGetOperands(invocation, out var left, out var right)
                && IsCorrectCall(right, c.SemanticModel)
                && c.SemanticModel.GetTypeInfo(left).Type is { } type
                && (type.DerivesFrom(KnownType.System_Array)
                    || type.DerivesFrom(KnownType.System_Collections_Immutable_ImmutableList_T)
                    || (type.DerivesFrom(KnownType.System_Collections_Generic_List_T) && !IsValueEquality(invocation)))) // This check avoids overlapping with S6617
            {
                c.ReportIssue(Diagnostic.Create(Rule, GetIdentifier(invocation)?.GetLocation()));
            }
        }, Language.SyntaxKind.InvocationExpression);

    internal static bool IsCorrectCall(SyntaxNode right, SemanticModel model) =>
        model.GetSymbolInfo(right).Symbol is IMethodSymbol method
        && method.IsExtensionOn(KnownType.System_Collections_Generic_IEnumerable_T);
}
