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

using Microsoft.CodeAnalysis.CodeFixes;
using SonarAnalyzer.CodeFixContext;

namespace SonarAnalyzer.Rules.CSharp
{
    [ExportCodeFixProvider(LanguageNames.CSharp)]
    public sealed class LinkedListPropertiesInsteadOfMethodsCodeFix : SonarCodeFix
    {
        private const string Title = "Replace extension method call with property";

        public override ImmutableArray<string> FixableDiagnosticIds { get; } =
            ImmutableArray.Create(LinkedListPropertiesInsteadOfMethods.DiagnosticId);

        protected override Task RegisterCodeFixesAsync(SyntaxNode root, SonarCodeFixContext context)
        {
            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;
            var identifierSyntax = (IdentifierNameSyntax)root.FindNode(diagnosticSpan, getInnermostNodeForTie: true);

            if (identifierSyntax is { Parent: ExpressionSyntax { Parent: InvocationExpressionSyntax invocationExpression } expression })
            {
                var newMember = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expression, SyntaxFactory.IdentifierName("Value"));
                context.RegisterCodeFix(
                    Title,
                    _ =>
                    {
                        var newRoot = root.ReplaceNode(invocationExpression, newMember);
                        return Task.FromResult(context.Document.WithSyntaxRoot(newRoot));
                    },
                    context.Diagnostics);
            }
            return Task.CompletedTask;
        }
    }
}
