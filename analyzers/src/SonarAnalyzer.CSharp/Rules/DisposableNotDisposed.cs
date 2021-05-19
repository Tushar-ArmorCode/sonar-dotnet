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
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;
using StyleCop.Analyzers.Lightup;

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [Rule(DiagnosticId)]
    public sealed class DisposableNotDisposed : SonarDiagnosticAnalyzer
    {
        internal const string DiagnosticId = "S2930";
        private const string MessageFormat = "Dispose '{0}' when it is no longer needed.";

        private static readonly DiagnosticDescriptor rule =
            DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, RspecStrings.ResourceManager);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(rule);

        private static readonly ImmutableArray<KnownType> TrackedTypes =
            ImmutableArray.Create(
                KnownType.System_IO_FileStream,
                KnownType.System_IO_StreamReader,
                KnownType.System_IO_StreamWriter,

                KnownType.System_Net_WebClient,

                KnownType.System_Net_Sockets_TcpClient,
                KnownType.System_Net_Sockets_UdpClient,

                KnownType.System_Drawing_Image,
                KnownType.System_Drawing_Bitmap
            );

        private static readonly ISet<string> DisposeMethods = new HashSet<string> { "Dispose", "Close" };

        private static readonly ISet<string> FactoryMethods = new HashSet<string>
        {
            "System.IO.File.Create",
            "System.IO.File.Open",
            "System.Drawing.Image.FromFile",
            "System.Drawing.Image.FromStream"
        };

        private class NodeAndSymbol
        {
            public SyntaxNode Node { get; set; }
            public ISymbol Symbol { get; set; }
        }

        protected override void Initialize(SonarAnalysisContext context)
        {
            context.RegisterSymbolAction(
                c =>
                {
                    var namedType = (INamedTypeSymbol)c.Symbol;
                    if (namedType.ContainingType != null ||
                        !namedType.IsClassOrStruct())
                    {
                        return;
                    }

                    var typesDeclarationsAndSemanticModels =
                        namedType.DeclaringSyntaxReferences
                        .Select(r => new SyntaxNodeAndSemanticModel<SyntaxNode>
                        {
                            SyntaxNode = r.GetSyntax(),
                            SemanticModel = c.Compilation.GetSemanticModel(r.SyntaxTree)
                        })
                        .ToList();

                    var trackedNodesAndSymbols = new HashSet<NodeAndSymbol>();
                    foreach (var typeDeclarationAndSemanticModel in typesDeclarationsAndSemanticModels)
                    {
                        TrackInitializedLocalsAndPrivateFields(typeDeclarationAndSemanticModel.SyntaxNode, typeDeclarationAndSemanticModel.SemanticModel, trackedNodesAndSymbols);
                        TrackAssignmentsToLocalsAndPrivateFields(typeDeclarationAndSemanticModel.SyntaxNode, typeDeclarationAndSemanticModel.SemanticModel, trackedNodesAndSymbols);
                    }

                    if (trackedNodesAndSymbols.Any())
                    {
                        var excludedSymbols = new HashSet<ISymbol>();
                        foreach (var typeDeclarationAndSemanticModel in typesDeclarationsAndSemanticModels)
                        {
                            ExcludeDisposedAndClosedLocalsAndPrivateFields(typeDeclarationAndSemanticModel.SyntaxNode, typeDeclarationAndSemanticModel.SemanticModel, excludedSymbols);
                            ExcludeReturnedPassedAndAliasedLocalsAndPrivateFields(typeDeclarationAndSemanticModel.SyntaxNode, typeDeclarationAndSemanticModel.SemanticModel, excludedSymbols);
                        }

                        foreach (var trackedNodeAndSymbol in trackedNodesAndSymbols)
                        {
                            if (!excludedSymbols.Contains(trackedNodeAndSymbol.Symbol))
                            {
                                c.ReportDiagnosticIfNonGenerated(Diagnostic.Create(rule, trackedNodeAndSymbol.Node.GetLocation(), trackedNodeAndSymbol.Symbol.Name));
                            }
                        }
                    }
                },
                SymbolKind.NamedType);
        }

        private static void TrackInitializedLocalsAndPrivateFields(SyntaxNode typeDeclaration, SemanticModel semanticModel, ISet<NodeAndSymbol> trackedNodesAndSymbols)
        {
            var localVariableDeclarations = typeDeclaration
                .DescendantNodes()
                .OfType<LocalDeclarationStatementSyntax>()
                .Where(localDeclaration => !localDeclaration.UsingKeyword().IsKind(SyntaxKind.UsingKeyword))
                .Select(localDeclaration => localDeclaration.Declaration);

            var fieldVariableDeclarations = typeDeclaration
                .DescendantNodes()
                .OfType<FieldDeclarationSyntax>()
                .Where(fieldDeclaration => !fieldDeclaration.Modifiers.Any() || fieldDeclaration.Modifiers.Any(SyntaxKind.PrivateKeyword))
                .Select(fieldDeclaration => fieldDeclaration.Declaration);

            var variableDeclarations = localVariableDeclarations.Concat(fieldVariableDeclarations);

            foreach (var declaration in variableDeclarations)
            {
                var trackedVariables = declaration.Variables.Where(v => v.Initializer != null && IsInstantiation(v.Initializer.Value, semanticModel));
                foreach (var variableNode in trackedVariables)
                {
                    trackedNodesAndSymbols.Add(new NodeAndSymbol { Node = variableNode, Symbol = semanticModel.GetDeclaredSymbol(variableNode) });
                }
            }
        }

        private static void TrackAssignmentsToLocalsAndPrivateFields(SyntaxNode typeDeclaration, SemanticModel semanticModel, ISet<NodeAndSymbol> trackedNodesAndSymbols)
        {
            var simpleAssignments = typeDeclaration
                .DescendantNodes()
                .Where(n => n.IsKind(SyntaxKind.SimpleAssignmentExpression))
                .Cast<AssignmentExpressionSyntax>();

            foreach (var simpleAssignment in simpleAssignments)
            {
                if (simpleAssignment.Parent.IsKind(SyntaxKind.UsingStatement))
                {
                    continue;
                }

                if (!IsInstantiation(simpleAssignment.Right, semanticModel))
                {
                    continue;
                }

                var referencedSymbol = semanticModel.GetSymbolInfo(simpleAssignment.Left).Symbol;
                if (referencedSymbol != null && IsLocalOrPrivateField(referencedSymbol))
                {
                    trackedNodesAndSymbols.Add(new NodeAndSymbol { Node = simpleAssignment, Symbol = referencedSymbol });
                }
            }
        }

        private static bool IsLocalOrPrivateField(ISymbol symbol)
        {
            return symbol.Kind == SymbolKind.Local ||
                (symbol.Kind == SymbolKind.Field && symbol.DeclaredAccessibility == Accessibility.Private);
        }

        private static void ExcludeDisposedAndClosedLocalsAndPrivateFields(SyntaxNode typeDeclaration, SemanticModel semanticModel, ISet<ISymbol> excludedSymbols)
        {
            var invocationsAndConditionalAccesses = typeDeclaration
                .DescendantNodes()
                .Where(n => n.IsKind(SyntaxKind.InvocationExpression) || n.IsKind(SyntaxKind.ConditionalAccessExpression));

            foreach (var invocationOrConditionalAccess in invocationsAndConditionalAccesses)
            {
                SimpleNameSyntax name;
                ExpressionSyntax expression;

                if (invocationOrConditionalAccess.IsKind(SyntaxKind.InvocationExpression))
                {
                    var invocation = (InvocationExpressionSyntax)invocationOrConditionalAccess;
                    var memberAccessNode = invocation.Expression as MemberAccessExpressionSyntax;

                    name = memberAccessNode?.Name;
                    expression = memberAccessNode?.Expression;
                }
                else if (invocationOrConditionalAccess.IsKind(SyntaxKind.ConditionalAccessExpression))
                {
                    var conditionalAccess = (ConditionalAccessExpressionSyntax)invocationOrConditionalAccess;
                    if (!(conditionalAccess.WhenNotNull is InvocationExpressionSyntax invocation))
                    {
                        continue;
                    }

                    var memberBindingNode = invocation.Expression as MemberBindingExpressionSyntax;

                    name = memberBindingNode?.Name;
                    expression = conditionalAccess.Expression;
                }
                else
                {
                    throw new NotSupportedException("Syntax node should be either an invocation or a conditional access expression");
                }

                if (name == null || !DisposeMethods.Contains(name.Identifier.Text))
                {
                    continue;
                }

                var referencedSymbol = semanticModel.GetSymbolInfo(expression).Symbol;
                if (referencedSymbol != null && IsLocalOrPrivateField(referencedSymbol))
                {
                    excludedSymbols.Add(referencedSymbol);
                }
            }
        }

        private static void ExcludeReturnedPassedAndAliasedLocalsAndPrivateFields(SyntaxNode typeDeclaration, SemanticModel semanticModel, ISet<ISymbol> excludedSymbols)
        {
            var identifiersAndSimpleMemberAccesses = typeDeclaration
                .DescendantNodes()
                .Where(n => n.IsKind(SyntaxKind.IdentifierName) || n.IsKind(SyntaxKind.SimpleMemberAccessExpression));

            foreach (var identifierOrSimpleMemberAccess in identifiersAndSimpleMemberAccesses)
            {
                ExpressionSyntax expression;
                if (identifierOrSimpleMemberAccess.IsKind(SyntaxKind.IdentifierName))
                {
                    expression = (IdentifierNameSyntax)identifierOrSimpleMemberAccess;
                }
                else if (identifierOrSimpleMemberAccess.IsKind(SyntaxKind.SimpleMemberAccessExpression))
                {

                    var memberAccess = (MemberAccessExpressionSyntax)identifierOrSimpleMemberAccess;
                    if (!memberAccess.Expression.IsKind(SyntaxKind.ThisExpression))
                    {
                        continue;
                    }
                    expression = memberAccess;
                }
                else
                {
                    throw new NotSupportedException("Syntax node should be either an identifier or a simple member access expression");
                }

                if (!IsStandaloneExpression(expression))
                {
                    continue;
                }

                var referencedSymbol = semanticModel.GetSymbolInfo(identifierOrSimpleMemberAccess).Symbol;
                if (referencedSymbol != null && IsLocalOrPrivateField(referencedSymbol))
                {
                    excludedSymbols.Add(referencedSymbol);
                }
            }
        }

        private static bool IsStandaloneExpression(ExpressionSyntax expression)
        {
            var parentAsAssignment = expression.Parent as AssignmentExpressionSyntax;

            return !(expression.Parent is ExpressionSyntax) ||
                (parentAsAssignment != null && object.ReferenceEquals(expression, parentAsAssignment.Right));
        }

        private static bool IsInstantiation(ExpressionSyntax expression, SemanticModel semanticModel)
        {
            return IsNewTrackedTypeObjectCreation(expression, semanticModel) ||
                IsDisposableRefStructCreation(expression, semanticModel) ||
                IsFactoryMethodInvocation(expression, semanticModel);
        }

        private static bool IsNewTrackedTypeObjectCreation(ExpressionSyntax expression, SemanticModel semanticModel)
        {
            if (!expression.IsKind(SyntaxKind.ObjectCreationExpression))
            {
                return false;
            }

            var type = semanticModel.GetTypeInfo(expression).Type;
            if (!type.IsAny(TrackedTypes))
            {
                return false;
            }

            return semanticModel.GetSymbolInfo(expression).Symbol is IMethodSymbol constructor && !constructor.Parameters.Any(p => p.Type.Implements(KnownType.System_IDisposable));
        }

        private static bool IsDisposableRefStructCreation(ExpressionSyntax expression, SemanticModel semanticModel)
        {
            if (!expression.IsKind(SyntaxKind.ObjectCreationExpression))
            {
                return false;
            }

            var type = semanticModel.GetTypeInfo(expression).Type;
            return type.IsStruct()
                && type.IsRefLikeType()
                && type.GetMembers().OfType<IMethodSymbol>().Any(methodSymbol => methodSymbol.Name == "Dispose");
        }

        private static bool IsFactoryMethodInvocation(ExpressionSyntax expression, SemanticModel semanticModel)
        {
            if (!(expression is InvocationExpressionSyntax invocation))
            {
                return false;
            }

            if (!(semanticModel.GetSymbolInfo(invocation).Symbol is IMethodSymbol methodSymbol))
            {
                return false;
            }

            var methodQualifiedName = methodSymbol.ContainingType.ToDisplayString() + "." + methodSymbol.Name;
            return FactoryMethods.Contains(methodQualifiedName);
        }
    }
}
