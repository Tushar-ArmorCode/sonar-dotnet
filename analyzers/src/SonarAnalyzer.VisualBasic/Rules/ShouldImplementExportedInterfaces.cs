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

using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.VisualBasic;
using Microsoft.CodeAnalysis.VisualBasic.Syntax;
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;
using SonarAnalyzer.Rules.Common;

namespace SonarAnalyzer.Rules.VisualBasic
{
    [DiagnosticAnalyzer(LanguageNames.VisualBasic)]
    [Rule(DiagnosticId)]
    public sealed class ShouldImplementExportedInterfaces : ShouldImplementExportedInterfacesBase<ArgumentSyntax, ExpressionSyntax, AttributeSyntax, SyntaxKind>
    {
        protected override SyntaxKind[] SyntaxKinds => new[] { SyntaxKind.Attribute };
        protected override ILanguageFacade<SyntaxKind> Language => VisualBasicFacade.Instance;

        protected override SeparatedSyntaxList<ArgumentSyntax>? GetAttributeArguments(AttributeSyntax attributeSyntax) =>
            attributeSyntax.ArgumentList?.Arguments;

        protected override SyntaxNode GetAttributeName(AttributeSyntax attributeSyntax) =>
            attributeSyntax.Name;

        protected override bool IsClassOrRecordSyntax(SyntaxNode syntaxNode) =>
            syntaxNode.IsKind(SyntaxKind.ClassStatement);

        protected override string GetIdentifier(ArgumentSyntax argumentSyntax) =>
            (argumentSyntax as SimpleArgumentSyntax)?.NameColonEquals?.Name.Identifier.ValueText;

        protected override ExpressionSyntax GetExpression(ArgumentSyntax argumentSyntax) =>
            argumentSyntax?.GetExpression();

        protected override SyntaxNode GetTypeOfOrGetTypeExpression(ExpressionSyntax expressionSyntax) =>
            (expressionSyntax as GetTypeExpressionSyntax)?.Type;
    }
}
