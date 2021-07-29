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

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SonarAnalyzer.Extensions;

namespace SonarAnalyzer.Helpers
{
    public class CSharpConstantValueFinder : ConstantValueFinder<IdentifierNameSyntax, VariableDeclaratorSyntax>
    {
        public CSharpConstantValueFinder(SemanticModel semanticModel) : base(semanticModel, new CSharpAssignmentFinder(), (int)SyntaxKind.NullLiteralExpression) { }

        protected override string IdentifierName(IdentifierNameSyntax node) =>
            node.Identifier.ValueText;

        protected override SyntaxNode InitializerValue(VariableDeclaratorSyntax node) =>
            node.Initializer?.Value;

        protected override VariableDeclaratorSyntax VariableDeclarator(SyntaxNode node) =>
            node as VariableDeclaratorSyntax;

        protected override bool IsPtrZero(SyntaxNode node) =>
            node is MemberAccessExpressionSyntax memberAccess
            && memberAccess.IsPtrZero(SemanticModel);
    }
}
