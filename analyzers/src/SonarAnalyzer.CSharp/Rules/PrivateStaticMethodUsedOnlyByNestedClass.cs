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

using SonarAnalyzer.CFG.Helpers;

namespace SonarAnalyzer.Rules.CSharp;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class PrivateStaticMethodUsedOnlyByNestedClass : SonarDiagnosticAnalyzer
{
    private const string DiagnosticId = "S3398";
    private const string MessageFormat = "Move the method inside '{0}'.";

    private static readonly SyntaxKind[] AnalyzedSyntaxKinds = new[]
    {
        SyntaxKind.ClassDeclaration,
        SyntaxKind.StructDeclaration,
        SyntaxKind.InterfaceDeclaration,
        SyntaxKindEx.RecordClassDeclaration,
        SyntaxKindEx.RecordStructDeclaration
    };

    private static readonly DiagnosticDescriptor Rule = DescriptorFactory.Create(DiagnosticId, MessageFormat);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

    protected override void Initialize(SonarAnalysisContext context) =>
        context.RegisterNodeAction(c =>
            {
                var declaredType = (TypeDeclarationSyntax)c.Node;

                if (!IsPartial(declaredType)
                    && HasNestedTypeDeclarations(declaredType)
                    && PrivateStaticMethodsOf(declaredType) is { Length: > 0 } candidates)
                {
                    // TODO: naming (type hierarchy)
                    var methodReferences = MethodReferencesInsideType(candidates, declaredType, c.SemanticModel);

                    foreach (var reference in methodReferences)
                    {
                        var typeToMoveInto = LowestCommonAncestorOrSelf(reference.Types);
                        if (typeToMoveInto != declaredType)
                        {
                            var nestedTypeName = typeToMoveInto.Identifier.ValueText;
                            c.ReportIssue(Diagnostic.Create(Rule, reference.Method.Identifier.GetLocation(), nestedTypeName));
                        }
                    }
                }
            },
            AnalyzedSyntaxKinds);

    private static MethodDeclarationSyntax[] PrivateStaticMethodsOf(TypeDeclarationSyntax type) =>
        type.Members
                .OfType<MethodDeclarationSyntax>()
                .Where(x => IsPrivateAndStatic(x, type))
                .ToArray();

    private static bool HasNestedTypeDeclarations(TypeDeclarationSyntax type) =>
        type.Members
                .OfType<TypeDeclarationSyntax>()
                .Any();

    private static bool IsPrivateAndStatic(MethodDeclarationSyntax method, TypeDeclarationSyntax containingType) =>
        method.Modifiers.Any(x => x.IsKind(SyntaxKind.StaticKeyword))
        && ((HasAnyModifier(method, SyntaxKind.PrivateKeyword) && !HasAnyModifier(method, SyntaxKind.ProtectedKeyword))
           || (!HasAnyModifier(method, SyntaxKind.PublicKeyword, SyntaxKind.ProtectedKeyword, SyntaxKind.InternalKeyword) && IsClassOrRecordClassOrInterfaceDeclaration(containingType)));

    private static bool IsPartial(TypeDeclarationSyntax type) =>
        type.Modifiers.Any(x => x.IsKind(SyntaxKind.PartialKeyword));

    private static bool IsClassOrRecordClassOrInterfaceDeclaration(TypeDeclarationSyntax type) =>
        type is ClassDeclarationSyntax or InterfaceDeclarationSyntax
        || (RecordDeclarationSyntaxWrapper.IsInstance(type) && !((RecordDeclarationSyntaxWrapper)type).ClassOrStructKeyword.IsKind(SyntaxKind.StructKeyword));

    private static bool HasAnyModifier(MethodDeclarationSyntax method, params SyntaxKind[] modifiers) =>
        method.Modifiers.Any(x => x.IsAnyKind(modifiers));

    private static TypeDeclarationSyntax LowestCommonAncestorOrSelf(IEnumerable<TypeDeclarationSyntax> declaredTypes)
    {
        var treePaths = declaredTypes.Select(PathFromTop);
        int minPathLength = treePaths.Select(x => x.Length).Min();
        var firstPath = treePaths.First();

        for (int i = 0; i < minPathLength; i++)
        {
            if (!treePaths.All(x => x[i] == firstPath[i]))
            {
                return firstPath[i - 1];
            }
        }

        return firstPath[minPathLength - 1];

        static TypeDeclarationSyntax[] PathFromTop(SyntaxNode node) =>
            node.AncestorsAndSelf()
                .OfType<TypeDeclarationSyntax>()
                .Distinct()
                .Reverse()
                .ToArray();
    }

    private static IEnumerable<MethodUsedByTypes> MethodReferencesInsideType(
        IEnumerable<MethodDeclarationSyntax> methods, TypeDeclarationSyntax outerType, SemanticModel model)
    {
        var collector = new PotentialMethodReferenceCollector(methods);
        collector.Visit(outerType);

        return collector.PotentialMethodReferences
                            .Where(x => !OnlyUsedByOuterType(x))
                            .Select(ResolveReferences)
                            .Where(x => x.Types.Any())
                            .ToArray();

        MethodUsedByTypes ResolveReferences(MethodWithPotentialReferences m)
        {
            var methodSymbol = model.GetDeclaredSymbol(m.Method);

            var typesWhichUseTheMethod = m.PotentialReferences
                .Where(x =>
                    !IsRecursiveMethodCall(x, m.Method)
                    && model.GetSymbolOrCandidateSymbol(x) is IMethodSymbol { } methodReference
                    && (methodReference == methodSymbol || methodReference.ConstructedFrom == methodSymbol))
                .Select(ContainingTypeDeclaration)
                .Distinct()
                .ToArray();

            return new MethodUsedByTypes(m.Method, typesWhichUseTheMethod);
        }

        bool IsRecursiveMethodCall(IdentifierNameSyntax methodCall, MethodDeclarationSyntax methodDeclaration) =>
            methodCall.Ancestors().OfType<MethodDeclarationSyntax>().FirstOrDefault() == methodDeclaration;

        bool OnlyUsedByOuterType(MethodWithPotentialReferences m) =>
            m.PotentialReferences.All(x => ContainingTypeDeclaration(x) == outerType);
    }

    private static TypeDeclarationSyntax ContainingTypeDeclaration(IdentifierNameSyntax identifier) =>
        identifier
            .Ancestors()
            .OfType<TypeDeclarationSyntax>()
            .First();

    private record MethodWithPotentialReferences(MethodDeclarationSyntax Method, IdentifierNameSyntax[] PotentialReferences);
    private record MethodUsedByTypes(MethodDeclarationSyntax Method, TypeDeclarationSyntax[] Types);

    /// <summary>
    /// Collects all the potential references to a set of methods inside the given syntax node.
    /// The collector looks for identifiers which match any of the methods' names, but does not try to resolve them to symbols with the semantic model.
    /// </summary>
    private class PotentialMethodReferenceCollector : CSharpSyntaxWalker
    {
        private readonly ISet<MethodDeclarationSyntax> methodsToFind;
        private readonly Dictionary<MethodDeclarationSyntax, List<IdentifierNameSyntax>> potentialMethodReferences;

        public IEnumerable<MethodWithPotentialReferences> PotentialMethodReferences =>
            potentialMethodReferences.Select(x => new MethodWithPotentialReferences(x.Key, x.Value.ToArray()));

        public PotentialMethodReferenceCollector(IEnumerable<MethodDeclarationSyntax> methodsToFind)
        {
            this.methodsToFind = new HashSet<MethodDeclarationSyntax>(methodsToFind);
            potentialMethodReferences = new();
        }

        public override void VisitIdentifierName(IdentifierNameSyntax identifier)
        {
            if (methodsToFind.FirstOrDefault(x => x.Identifier.ValueText == identifier.Identifier.ValueText) is { } method)
            {
                var referenceList = potentialMethodReferences.GetOrAdd(method, _ => new());
                referenceList.Add(identifier);
            }
        }
    }
}
