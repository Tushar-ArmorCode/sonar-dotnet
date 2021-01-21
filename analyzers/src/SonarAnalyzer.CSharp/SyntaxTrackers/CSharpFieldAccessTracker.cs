﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2021 SonarSource SA
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

using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SonarAnalyzer.Common;

namespace SonarAnalyzer.Helpers
{
    public class CSharpFieldAccessTracker : FieldAccessTracker<SyntaxKind>
    {
        protected override SyntaxKind[] TrackedSyntaxKinds { get; } =
            new[]
            {
                SyntaxKind.SimpleMemberAccessExpression,
                SyntaxKind.MemberBindingExpression,
                SyntaxKind.IdentifierName
            };
        protected override GeneratedCodeRecognizer GeneratedCodeRecognizer { get; } = CSharpGeneratedCodeRecognizer.Instance;

        public CSharpFieldAccessTracker(IAnalyzerConfiguration analyzerConfiguration, DiagnosticDescriptor rule) : base(analyzerConfiguration, rule) { }

        public override Condition WhenRead() =>
            context => !((ExpressionSyntax)context.Node).IsLeftSideOfAssignment();

        public override Condition MatchSet() =>
            context => ((ExpressionSyntax)context.Node).IsLeftSideOfAssignment();

        public override Condition AssignedValueIsConstant() =>
            context =>
            {
                var assignment = (AssignmentExpressionSyntax)context.Node.Ancestors()
                    .FirstOrDefault(ancestor => ancestor.IsKind(SyntaxKind.SimpleAssignmentExpression));

                return assignment != null &&
                    assignment.Right.HasConstantValue(context.SemanticModel);
            };

        protected override string GetFieldName(SyntaxNode expression) =>
            ((ExpressionSyntax)expression).GetIdentifier()?.Identifier.ValueText;

        protected override bool IsIdentifierWithinMemberAccess(SyntaxNode expression) =>
            expression.IsKind(SyntaxKind.IdentifierName)
            && expression.Parent.IsKind(SyntaxKind.SimpleMemberAccessExpression);
    }
}
