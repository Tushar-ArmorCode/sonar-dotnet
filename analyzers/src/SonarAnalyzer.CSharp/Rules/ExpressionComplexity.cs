﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2022 SonarSource SA
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

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public sealed class ExpressionComplexity : ExpressionComplexityBase<SyntaxKind>
    {
        protected override ILanguageFacade Language { get; } = CSharpFacade.Instance;

        protected override SyntaxKind[] TransparentKinds { get; } =
            {
                SyntaxKind.ParenthesizedExpression,
                SyntaxKind.LogicalNotExpression,
                SyntaxKindEx.ParenthesizedPattern,
                SyntaxKindEx.NotPattern,
            };

        protected override SyntaxKind[] ComplexityIncreasingKinds { get; } =
            {
                SyntaxKind.ConditionalExpression,
                SyntaxKind.LogicalAndExpression,
                SyntaxKind.LogicalOrExpression,
                SyntaxKindEx.CoalesceAssignmentExpression,
                SyntaxKindEx.AndPattern,
                SyntaxKindEx.OrPattern
            };

        protected override SyntaxNode[] ExpressionChildren(SyntaxNode node) =>
            node switch
            {
                ConditionalExpressionSyntax conditional => new[] { conditional.WhenTrue, conditional.WhenFalse },
                BinaryExpressionSyntax { RawKind: (int)SyntaxKind.LogicalAndExpression or (int)SyntaxKind.LogicalOrExpression } binary => new[] { binary.Left, binary.Right },
                { RawKind: (int)SyntaxKindEx.AndPattern or (int)SyntaxKindEx.OrPattern } pattern when (BinaryPatternSyntaxWrapper)pattern is var patternWrapper =>
                    new[] { patternWrapper.Left.SyntaxNode, patternWrapper.Right.SyntaxNode },
                AssignmentExpressionSyntax { RawKind: (int)SyntaxKindEx.CoalesceAssignmentExpression } assigment => new[] { assigment.Left, assigment.Right },

                ParenthesizedExpressionSyntax { Expression: { } expression } => new[] { expression },
                PrefixUnaryExpressionSyntax { RawKind: (int)SyntaxKind.LogicalNotExpression, Operand: { } operand } => new[] { operand },
                { RawKind: (int)SyntaxKindEx.ParenthesizedPattern } parenthesized when (ParenthesizedPatternSyntaxWrapper)parenthesized is var parenthesizedWrapped =>
                    new[] { parenthesizedWrapped.Pattern.SyntaxNode },
                { RawKind: (int)SyntaxKindEx.NotPattern } negated when (UnaryPatternSyntaxWrapper)negated is var negatedWrapper =>
                    new[] { negatedWrapper.Pattern.SyntaxNode },
                _ => Array.Empty<SyntaxNode>(),
            };
    }
}
