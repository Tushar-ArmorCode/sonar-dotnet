﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2018 SonarSource SA
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
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [Rule(DiagnosticId)]
    public sealed class DoNotOverwriteCollectionElements
        : DoNotOverwriteCollectionElementsBase<InvocationExpressionSyntax, IdentifierNameSyntax,
            StatementSyntax, MemberAccessExpressionSyntax, ThisExpressionSyntax, BaseExpressionSyntax, ExpressionStatementSyntax>
    {
        private static readonly DiagnosticDescriptor rule =
            DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, RspecStrings.ResourceManager);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(rule);

        internal override DiagnosticDescriptor Rule => rule;

        protected override void Initialize(SonarAnalysisContext context)
        {
            context.RegisterSyntaxNodeActionInNonGenerated(
                c => VerifyAssignment(c),
                SyntaxKind.SimpleAssignmentExpression);

            context.RegisterSyntaxNodeActionInNonGenerated(
               c => VerifyInvocationExpression(c),
               SyntaxKind.InvocationExpression);
        }

        internal override KnownType DictionaryType => KnownType.System_Collections_Generic_IDictionary_TKey_TValue;

        protected override SyntaxNode GetExpression(ExpressionStatementSyntax expression) => expression.Expression;

        protected override SyntaxNode GetExpression(InvocationExpressionSyntax invocation) => invocation.Expression;

        protected override SyntaxNode GetExpression(MemberAccessExpressionSyntax memberAccess) => memberAccess.Expression;

        protected override SyntaxNode GetName(MemberAccessExpressionSyntax memberAccess) => memberAccess.Name;

        protected override SyntaxToken GetIdentifierToken(IdentifierNameSyntax invocation) => invocation.Identifier;

        protected override SyntaxToken GetIdentifierToken(MemberAccessExpressionSyntax memberAccess) => memberAccess.Name.Identifier;

        protected override SyntaxToken GetIdentifierToken(ThisExpressionSyntax thisAccess) => thisAccess.Token;

        protected override SyntaxToken GetIdentifierToken(BaseExpressionSyntax baseAccess) => baseAccess.Token;

        protected override bool HasNumberOfArguments(InvocationExpressionSyntax invocation, int number)
            => invocation.ArgumentList != null && invocation.ArgumentList.Arguments.Count == number;

        protected override SyntaxNode GetFirstArgument(InvocationExpressionSyntax invocation)
            => invocation.ArgumentList != null && invocation.ArgumentList.Arguments.Count >= 1
                ? invocation.ArgumentList.Arguments[0]
                : null;

        private void VerifyAssignment(SyntaxNodeAnalysisContext c)
        {
            var assignment = (AssignmentExpressionSyntax)c.Node;

            if (!(assignment.Left is ElementAccessExpressionSyntax elementAccess) ||
                elementAccess.ArgumentList == null ||
                elementAccess.ArgumentList.Arguments.Count == 0 ||
                !((elementAccess.Expression as IdentifierNameSyntax)?.Identifier.ValueText is string accessedOn))
            {
                return;
            }

            var arguments = elementAccess.ArgumentList.Arguments.Select(a => a.ToString());

            var previousAssignmentOfVariable = GetPreviousStatements(assignment)
                .OfType<ExpressionStatementSyntax>()
                .Select(ess => ess.Expression)
                .TakeWhile(e => IsElementAccessAssignmentOnSameItem(e, accessedOn))
                .Cast<AssignmentExpressionSyntax>()
                .Select(aes => aes.Left)
                .Cast<ElementAccessExpressionSyntax>()
                .FirstOrDefault(eaes => arguments.SequenceEqual(eaes.ArgumentList.Arguments.Select(a => a.ToString())));

            if (previousAssignmentOfVariable != null)
            {
                c.ReportDiagnosticWhenActive(Diagnostic.Create(rule, elementAccess.GetLocation(),
                    additionalLocations: new[] { previousAssignmentOfVariable.GetLocation() }));
            }
        }

        private static bool IsElementAccessAssignmentOnSameItem(ExpressionSyntax expression, string accessedOn) =>
            expression is AssignmentExpressionSyntax aes &&
            aes.Left is ElementAccessExpressionSyntax currentElementAccess &&
            currentElementAccess.ArgumentList != null &&
            currentElementAccess.Expression is IdentifierNameSyntax ins &&
            ins.Identifier.ValueText == accessedOn;
    }
}
