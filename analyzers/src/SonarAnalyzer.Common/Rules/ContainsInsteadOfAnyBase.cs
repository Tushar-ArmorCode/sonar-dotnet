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

public abstract class ContainsInsteadOfAnyBase<TSyntaxKind, TInvocationExpression> : InsteadOfAny<TSyntaxKind, TInvocationExpression>
    where TSyntaxKind : struct
    where TInvocationExpression : SyntaxNode
{
    private const string DiagnosticId = "S6617";

    protected override string MessageFormat => """Collection-specific "Contains" method should be used instead of the "Any" extension.""";

    protected override ImmutableArray<(KnownType Type, bool CheckContext)> RuleSpecificTypes { get; } = ImmutableArray.Create(
        (KnownType.System_Collections_Generic_List_T, true),
        (KnownType.System_Collections_Generic_HashSet_T, true),
        (KnownType.System_Collections_Generic_SortedSet_T, true));

    protected override bool IsInValidContext(TInvocationExpression invocation, SemanticModel model) =>
        IsSimpleEqualityCheck(invocation, model);

    protected ContainsInsteadOfAnyBase() : base(DiagnosticId) { }
}
