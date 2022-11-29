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

namespace SonarAnalyzer.Rules
{
    public abstract class ShiftDynamicNotIntegerBase<TSyntaxKind, TExpressionSyntax> : SonarDiagnosticAnalyzer<TSyntaxKind>
        where TSyntaxKind : struct
        where TExpressionSyntax : SyntaxNode
    {
        internal const string DiagnosticId = "S3449";

        protected abstract bool CanBeConvertedTo(TExpressionSyntax expression, ITypeSymbol type, SemanticModel semanticModel);

        protected abstract bool ShouldRaise(SemanticModel semanticModel, TExpressionSyntax left, TExpressionSyntax right);

        protected override string MessageFormat => "Remove this erroneous shift, it will fail because '{0}' can't be implicitly converted to 'int'.";

        protected ShiftDynamicNotIntegerBase() : base(DiagnosticId) { }

        protected void CheckExpressionWithTwoParts<T>(SyntaxNodeAnalysisContext context,
                                                      Func<T, TExpressionSyntax> getLeft,
                                                      Func<T, TExpressionSyntax> getRight)
                                                      where T : SyntaxNode
        {
            var expression = (T)context.Node;
            var left = getLeft(expression);
            var right = getRight(expression);

            if (!IsErrorType(right, context.SemanticModel, out var typeOfRight)
                && ShouldRaise(context.SemanticModel, left, right))
            {
                var typeInMessage = GetTypeNameForMessage(right, typeOfRight, context.SemanticModel);

                context.ReportIssue(
                    Diagnostic.Create(Rule, right.GetLocation(), typeInMessage));
            }
        }

        private static string GetTypeNameForMessage(SyntaxNode expression, ITypeSymbol typeOfRight, SemanticModel semanticModel) =>
            semanticModel.GetConstantValue(expression) is { HasValue: true, Value: null }
            ? "null"
            : typeOfRight.ToMinimalDisplayString(semanticModel, expression.SpanStart);

        private static bool IsErrorType(TExpressionSyntax expression, SemanticModel semanticModel, out ITypeSymbol type)
        {
            type = semanticModel.GetTypeInfo(expression).Type;
            return type.Is(TypeKind.Error);
        }

        protected bool IsConvertibleToInt(TExpressionSyntax expression, SemanticModel semanticModel) =>
            semanticModel.Compilation.GetTypeByMetadataName(KnownType.System_Int32) is { } intType
            && CanBeConvertedTo(expression, intType, semanticModel);
    }
}
