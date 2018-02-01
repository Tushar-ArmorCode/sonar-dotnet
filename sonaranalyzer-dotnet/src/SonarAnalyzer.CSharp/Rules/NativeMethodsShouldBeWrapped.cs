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

using System.Collections.Generic;
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
    public sealed class NativeMethodsShouldBeWrapped : SonarDiagnosticAnalyzer
    {
        internal const string DiagnosticId = "S4200";
        private const string MessageFormat = "{0}";
        private const string MakeThisMethodPrivateMessage = "Make this native method private and provide a wrapper.";
        private const string MakeThisWrapperLessTrivialMessage = "Make this wrapper for native method '{0}' less trivial.";

        private static readonly DiagnosticDescriptor rule =
            DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, RspecStrings.ResourceManager);
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(rule);

        private static readonly ISet<Accessibility> PublicOrProtected = new HashSet<Accessibility>
        {
            Accessibility.Public,
            Accessibility.Protected,
            Accessibility.ProtectedOrInternal,
            Accessibility.ProtectedAndInternal,
        };

        protected override void Initialize(SonarAnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(ReportPublicExternalMethods, SyntaxKind.MethodDeclaration);
            context.RegisterSyntaxNodeAction(ReportTrivialWrappers, SyntaxKind.MethodDeclaration);
        }

        private static void ReportPublicExternalMethods(SyntaxNodeAnalysisContext c)
        {
            var methodDeclaration = (MethodDeclarationSyntax)c.Node;
            var methodSymbol = c.SemanticModel.GetDeclaredSymbol(methodDeclaration);

            if (methodSymbol != null &&
                IsExternal(methodSymbol) &&
                IsPubliclyAccessible(methodSymbol))
            {
                c.ReportDiagnosticWhenActive(Diagnostic.Create(rule, methodDeclaration.Identifier.GetLocation(),
                    MakeThisMethodPrivateMessage));
            }
        }

        private static void ReportTrivialWrappers(SyntaxNodeAnalysisContext c)
        {
            var methodDeclaration = (MethodDeclarationSyntax)c.Node;
            var methodSymbol = c.SemanticModel.GetDeclaredSymbol(methodDeclaration);

            if (methodSymbol == null ||
                IsExternal(methodSymbol) ||
                methodDeclaration.ParameterList == null ||
                methodDeclaration.ParameterList.Parameters.Count == 0)
            {
                return;
            }

            var descendants = GetBodyDescendants(methodDeclaration);

            if (HasAtLeastTwo(descendants.OfType<StatementSyntax>()) ||
                HasAtLeastTwo(descendants.OfType<InvocationExpressionSyntax>()))
            {
                return;
            }

            var externalMethodSymbols = GetExternalMethods(methodSymbol);

            descendants.OfType<InvocationExpressionSyntax>()
                .Where(ParametersMatchContainingMethodDeclaration)
                .Select(i => c.SemanticModel.GetSymbolInfo(i).Symbol)
                .OfType<IMethodSymbol>()
                .Where(externalMethodSymbols.Contains)
                .ToList()
                .ForEach(Report);

            void Report(IMethodSymbol externMethod) =>
                c.ReportDiagnosticWhenActive(Diagnostic.Create(rule, methodDeclaration.Identifier.GetLocation(),
                  string.Format(MakeThisWrapperLessTrivialMessage, externMethod.Name)));

            bool ParametersMatchContainingMethodDeclaration(InvocationExpressionSyntax invocation) =>
                invocation.ArgumentList.Arguments.All(IsDeclaredParameterOrLiteral);

            bool IsDeclaredParameterOrLiteral(ArgumentSyntax a) =>
                a.Expression is LiteralExpressionSyntax ||
                a.Expression is IdentifierNameSyntax i
                    && methodDeclaration.ParameterList.Parameters.Any(p => p.Identifier.Text == i.Identifier.Text);
        }

        private static ISet<IMethodSymbol> GetExternalMethods(IMethodSymbol methodSymbol) =>
            methodSymbol.ContainingType.GetMembers()
                .OfType<IMethodSymbol>()
                .Where(IsExternal)
                .ToHashSet();

        private static IEnumerable<SyntaxNode> GetBodyDescendants(MethodDeclarationSyntax methodDeclaration) =>
            methodDeclaration.Body?.DescendantNodes()
            ?? methodDeclaration.ExpressionBody?.DescendantNodes()
            ?? Enumerable.Empty<SyntaxNode>();

        private static bool IsPubliclyAccessible(IMethodSymbol methodSymbol) =>
            PublicOrProtected.Contains(methodSymbol.GetEffectiveAccessibility());

        private static bool IsExternal(IMethodSymbol symbol) =>
            symbol.IsExtern;

        private static bool HasAtLeastTwo<T>(IEnumerable<T> collection) =>
            collection.Take(2).Count() == 2;
    }
}
