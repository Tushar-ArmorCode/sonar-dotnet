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

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.CFG.Roslyn;
using SonarAnalyzer.CFG.Sonar;
using SonarAnalyzer.Common;
using SonarAnalyzer.Extensions;
using SonarAnalyzer.Helpers;
using StyleCop.Analyzers.Lightup;
using SymbolWithInitializer = System.Collections.Generic.KeyValuePair<Microsoft.CodeAnalysis.ISymbol, Microsoft.CodeAnalysis.CSharp.Syntax.EqualsValueClauseSyntax>;

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [Rule(DiagnosticId)]
    public sealed partial class MemberInitializerRedundant : SonarDiagnosticAnalyzer
    {
        internal const string DiagnosticId = "S3604";
        private const string InstanceMemberMessage = "Remove the member initializer, all constructors set an initial value for the member.";
        private const string StaticMemberMessage = "Remove the static member initializer, a static constructor or module initializer sets an initial value for the member.";

        private readonly bool forceSonarCfg;

        private static readonly DiagnosticDescriptor Rule = DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, "{0}", RspecStrings.ResourceManager);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(Rule);

        public MemberInitializerRedundant() : this(AnalyzerConfiguration.AlwaysEnabled) { }

        internal /* for testing */ MemberInitializerRedundant(IAnalyzerConfiguration configuration) =>
            forceSonarCfg = !ControlFlowGraph.IsAvailable || configuration.ForceSonarCfg;

        protected override void Initialize(SonarAnalysisContext context) =>
            context.RegisterSyntaxNodeActionInNonGenerated(
                c =>
                {
                    var declaration = (TypeDeclarationSyntax)c.Node;
                    var members = c.SemanticModel.GetDeclaredSymbol(declaration)?.GetMembers();
                    if (members == null || members.Value.Length == 0)
                    {
                        return;
                    }

                    // structs cannot initialize fields/properties at declaration time
                    // interfaces cannot have instance fields and instance properties cannot have initializers
                    if (declaration is ClassDeclarationSyntax)
                    {
                        CheckInstanceMembers(c, declaration, members);
                    }
                    CheckStaticMembers(c, declaration, members);
                },
                // For record support, see details in https://github.com/SonarSource/sonar-dotnet/pull/4756
                // it is difficult to work with instance record constructors w/o raising FPs
                SyntaxKind.ClassDeclaration,
                SyntaxKind.StructDeclaration,
                SyntaxKind.InterfaceDeclaration);

        private void CheckInstanceMembers(SyntaxNodeAnalysisContext c, TypeDeclarationSyntax declaration, IEnumerable<ISymbol> typeMembers)
        {
            var typeInitializers = typeMembers.OfType<IMethodSymbol>().Where(x => x is { MethodKind: MethodKind.Constructor, IsStatic: false, IsImplicitlyDeclared: false }).ToList();
            if (typeInitializers.Count == 0)
            {
                return;
            }

            // only retrieve the member symbols (an expensive call) if there are explicit class initializers
            var initializedMembers = GetInitializedMembers(c.SemanticModel, declaration, field => !field.Modifiers.Any(IsStaticOrConst), property => !property.Modifiers.Any(IsStaticOrConst));
            if (initializedMembers.Count == 0)
            {
                return;
            }

            var initializerDeclarations = GetInitializerDeclarations<ConstructorDeclarationSyntax>(c, typeInitializers);
            foreach (var memberSymbol in initializedMembers.Keys)
            {
                // the instance member should be initialized in ALL instance constructors
                // otherwise, initializing it inline makes sense and the rule should not report
                if (initializerDeclarations.All(constructor =>
                    {
                        if (constructor.Node.Initializer != null
                            && constructor.Node.Initializer.ThisOrBaseKeyword.IsKind(SyntaxKind.ThisKeyword))
                        {
                            // Calls another ctor, which is also checked.
                            return true;
                        }

                        return IsSymbolFirstSetInCfg(memberSymbol, constructor.Node, constructor.SemanticModel, forceSonarCfg);
                    }))
                {
                    c.ReportDiagnosticWhenActive(Diagnostic.Create(Rule, initializedMembers[memberSymbol].GetLocation(), InstanceMemberMessage));
                }
            }
        }

        private void CheckStaticMembers(SyntaxNodeAnalysisContext c, TypeDeclarationSyntax declaration, IEnumerable<ISymbol> typeMembers)
        {
            var typeInitializers = typeMembers.OfType<IMethodSymbol>().Where(method => method.MethodKind == MethodKind.StaticConstructor || method.IsModuleInitializer()).ToList();
            if (typeInitializers.Count == 0)
            {
                return;
            }

            // only retrieve the member symbols (an expensive call) if there are explicit class initializers
            var initializedMembers = GetInitializedMembers(c.SemanticModel, declaration, field => field.Modifiers.Any(IsStatic), property => property.Modifiers.Any(IsStatic));
            if (initializedMembers.Count == 0)
            {
                return;
            }

            var initializerDeclarations = GetInitializerDeclarations<BaseMethodDeclarationSyntax>(c, typeInitializers);
            foreach (var memberSymbol in initializedMembers.Keys)
            {
                // there can be only one static constructor
                // all module initializers are executed when the type is created, so it is enough if ANY initializes the member
                if (initializerDeclarations.Any(x => IsSymbolFirstSetInCfg(memberSymbol, x.Node, x.SemanticModel, forceSonarCfg)))
                {
                    c.ReportDiagnosticWhenActive(Diagnostic.Create(Rule, initializedMembers[memberSymbol].GetLocation(), StaticMemberMessage));
                }
            }
        }

        // Retrieves the class members which are initialized - instance or static ones, depending on the given filter.
        // The returned dictionary has as key the member symbol and as value the initialization syntax.
        private static Dictionary<ISymbol, EqualsValueClauseSyntax> GetInitializedMembers(SemanticModel semanticModel,
                                                                                   TypeDeclarationSyntax declaration,
                                                                                   Func<BaseFieldDeclarationSyntax, bool> fieldFilter,
                                                                                   Func<PropertyDeclarationSyntax, bool> propertyFilter)
        {
            var candidateFields = GetInitializedFieldLikeDeclarations<FieldDeclarationSyntax, IFieldSymbol>(declaration, fieldFilter, semanticModel, f => f.Type);
            var candidateProperties = GetInitializedPropertyDeclarations(declaration, propertyFilter, semanticModel);
            var candidateEvents = GetInitializedFieldLikeDeclarations<EventFieldDeclarationSyntax, IEventSymbol>(declaration, fieldFilter, semanticModel, f => f.Type);
            var allMembers = candidateFields.Select(t => new SymbolWithInitializer(t.Symbol, t.Initializer))
                .Concat(candidateEvents.Select(t => new SymbolWithInitializer(t.Symbol, t.Initializer)))
                .Concat(candidateProperties.Select(t => new SymbolWithInitializer(t.Symbol, t.Initializer)))
                .ToDictionary(t => t.Key, t => t.Value);
            return allMembers;
        }

        private static bool IsStaticOrConst(SyntaxToken token) =>
            token.IsAnyKind(SyntaxKind.StaticKeyword, SyntaxKind.ConstKeyword);

        private static bool IsStatic(SyntaxToken token) =>
            token.IsKind(SyntaxKind.StaticKeyword);

        /// <summary>
        /// Returns true if the member is overwritten without being read in the instance constructor.
        /// Returns false if the member is not set in the constructor, or if it is read before being set.
        /// </summary>
        private static bool IsSymbolFirstSetInCfg(ISymbol classMember, BaseMethodDeclarationSyntax constructorOrInitializer, SemanticModel semanticModel, bool forceSonarCfg)
        {
            var body = (CSharpSyntaxNode)constructorOrInitializer.Body ?? constructorOrInitializer.ExpressionBody();
            if (forceSonarCfg)
            {
                if (!CSharpControlFlowGraph.TryGet(body, semanticModel, out var cfg))
                {
                    return false;
                }

                var checker = new MemberInitializerRedundancyCheckerSonar(cfg, classMember, semanticModel);
                return checker.CheckAllPaths();
            }
            else
            {
                var cfg = ControlFlowGraph.Create(body.Parent, semanticModel);
                var checker = new MemberInitializerRedundancyCheckerRoslyn(cfg, classMember, semanticModel);
                return checker.CheckAllPaths();
            }
        }

        private static List<NodeSymbolAndSemanticModel<TSyntax, IMethodSymbol>> GetInitializerDeclarations<TSyntax>(SyntaxNodeAnalysisContext context, List<IMethodSymbol> constructorSymbols)
                where TSyntax : SyntaxNode =>
                constructorSymbols
                    .Select(x => new NodeSymbolAndSemanticModel<TSyntax, IMethodSymbol>(null, x.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() as TSyntax, x))
                    .Where(x => x.Node != null)
                    .Select(x => new NodeSymbolAndSemanticModel<TSyntax, IMethodSymbol>(x.Node.EnsureCorrectSemanticModelOrDefault(context.SemanticModel), x.Node, x.Symbol))
                    .Where(x => x.SemanticModel != null)
                    .ToList();

        private static IEnumerable<DeclarationTuple<IPropertySymbol>> GetInitializedPropertyDeclarations(TypeDeclarationSyntax declaration,
                                                                                                         Func<PropertyDeclarationSyntax, bool> declarationFilter,
                                                                                                         SemanticModel semanticModel) =>
            declaration.Members
                .OfType<PropertyDeclarationSyntax>()
                .Where(p => declarationFilter(p)
                            && p.Initializer != null
                            && p.IsAutoProperty())
                .Select(p => new DeclarationTuple<IPropertySymbol>(p.Initializer, semanticModel.GetDeclaredSymbol(p)))
                .Where(t =>
                    t.Symbol != null
                    && !MemberInitializedToDefault.IsDefaultValueInitializer(t.Initializer, t.Symbol.Type));

        private static IEnumerable<DeclarationTuple<TSymbol>> GetInitializedFieldLikeDeclarations<TDeclarationType, TSymbol>(TypeDeclarationSyntax declaration,
                                                                                                                             Func<TDeclarationType, bool> declarationFilter,
                                                                                                                             SemanticModel semanticModel,
                                                                                                                             Func<TSymbol, ITypeSymbol> typeSelector)
            where TDeclarationType : BaseFieldDeclarationSyntax
            where TSymbol : class, ISymbol =>
            declaration.Members
                .OfType<TDeclarationType>()
                .Where(declarationFilter)
                .SelectMany(fd => fd.Declaration.Variables
                    .Where(v => v.Initializer != null)
                    .Select(v => new DeclarationTuple<TSymbol>(v.Initializer, semanticModel.GetDeclaredSymbol(v) as TSymbol)))
                .Where(t =>
                    t.Symbol != null
                    && !MemberInitializedToDefault.IsDefaultValueInitializer(t.Initializer, typeSelector(t.Symbol)));

        private class RedundancyChecker
        {
            private readonly ISymbol memberToCheck;
            private readonly SemanticModel semanticModel;

            public RedundancyChecker(ISymbol memberToCheck, SemanticModel semanticModel)
            {
                this.memberToCheck = memberToCheck;
                this.semanticModel = semanticModel;
            }

            public bool IsMemberUsedInsideLambda(SyntaxNode instruction) =>
                instruction.DescendantNodes()
                    .OfType<IdentifierNameSyntax>()
                    .Select(PossibleMemberAccessParent)
                    .Any(IsMatchingMember);

            public bool IsMatchingMember(ExpressionSyntax expression)
            {
                return ExtractIdentifier(expression) is { } identifier
                       && semanticModel.GetSymbolInfo(identifier).Symbol is { } assignedSymbol
                       && memberToCheck.Equals(assignedSymbol);

                IdentifierNameSyntax ExtractIdentifier(ExpressionSyntax expressionSyntax)
                {
                    if (expressionSyntax.IsKind(SyntaxKind.IdentifierName))
                    {
                        return (IdentifierNameSyntax)expressionSyntax;
                    }
                    if (expressionSyntax is MemberAccessExpressionSyntax memberAccess
                        && memberAccess.Expression.IsKind(SyntaxKind.ThisExpression))
                    {
                        return memberAccess.Name as IdentifierNameSyntax;
                    }
                    if (expressionSyntax is ConditionalAccessExpressionSyntax conditionalAccess
                        && conditionalAccess.Expression.IsKind(SyntaxKind.ThisExpression))
                    {
                        return (conditionalAccess.WhenNotNull as MemberBindingExpressionSyntax)?.Name as IdentifierNameSyntax;
                    }
                    return null;
                }
            }

            public static ExpressionSyntax PossibleMemberAccessParent(SyntaxNode node) =>
                node is MemberAccessExpressionSyntax memberAccess
                    ? memberAccess
                    : PossibleMemberAccessParent(node as IdentifierNameSyntax);

            private static ExpressionSyntax PossibleMemberAccessParent(IdentifierNameSyntax identifier)
            {
                if (identifier.Parent is MemberAccessExpressionSyntax memberAccess)
                {
                    return memberAccess;
                }

                if (identifier.Parent is MemberBindingExpressionSyntax memberBinding)
                {
                    return (ExpressionSyntax)memberBinding.Parent;
                }

                return identifier;
            }

            public bool TryGetReadWriteFromMemberAccess(ExpressionSyntax expression, out bool isRead)
            {
                isRead = false;

                var parenthesized = expression.GetSelfOrTopParenthesizedExpression();

                if (!IsMatchingMember(expression))
                {
                    return false;
                }

                if (IsOutArgument(parenthesized))
                {
                    isRead = false;
                    return true;
                }

                if (IsReadAccess(parenthesized, this.semanticModel))
                {
                    isRead = true;
                    return true;
                }

                return false;
            }

            private static bool IsBeingAssigned(ExpressionSyntax expression) =>
                expression.Parent is AssignmentExpressionSyntax assignment
                && assignment.IsKind(SyntaxKind.SimpleAssignmentExpression)
                && assignment.Left == expression;

            private static bool IsOutArgument(ExpressionSyntax parenthesized) =>
                parenthesized.Parent is ArgumentSyntax argument
                && argument.RefOrOutKeyword.IsKind(SyntaxKind.OutKeyword);

            private static bool IsReadAccess(ExpressionSyntax parenthesized, SemanticModel semanticModel) =>
                !IsBeingAssigned(parenthesized)
                && !parenthesized.IsInNameOfArgument(semanticModel);
        }

        private class DeclarationTuple<TSymbol>
            where TSymbol : ISymbol
        {
            public EqualsValueClauseSyntax Initializer { get; set; }
            public TSymbol Symbol { get; set; }

            public DeclarationTuple(EqualsValueClauseSyntax initializer, TSymbol symbol)
            {
                Initializer = initializer;
                Symbol = symbol;
            }
        }
    }
}
