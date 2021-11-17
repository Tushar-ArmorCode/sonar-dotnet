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
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.Rules
{
    public abstract class DoNotCheckZeroSizeCollectionBase<TSyntaxKind> : SonarDiagnosticAnalyzer
        where TSyntaxKind : struct
    {
        protected const string DiagnosticId = "S3981";
        private const string MessageFormat = "The '{0}' of '{1}' always evaluates as '{2}' regardless the size.";
        private const string CountName = nameof(Enumerable.Count);
        private const string LengthName = nameof(Array.Length);
        private const string LongLengthName = nameof(Array.LongLength);
        private readonly DiagnosticDescriptor rule;

        protected abstract ILanguageFacade<TSyntaxKind> Language { get; }
        protected abstract string IEnumerableTString { get; }

        protected DoNotCheckZeroSizeCollectionBase() =>
            rule = DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, Language.RspecResources);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(rule);

        protected override void Initialize(SonarAnalysisContext context) =>
            context.RegisterSyntaxNodeActionInNonGenerated(Language.GeneratedCodeRecognizer,
                c =>
                {
                    var binary = c.Node;
                    var binaryLeft = Language.Syntax.BinaryExpressionLeft(binary);
                    var binaryRight = Language.Syntax.BinaryExpressionRight(binary);

                    if (Language.ExpressionNumericConverter.TryGetConstantIntValue(c.SemanticModel, binaryLeft, out var left))
                    {
                        CheckExpression(c, binary, binaryRight, left, Language.Syntax.ComparisonKind(binary).Mirror());
                    }
                    else if (Language.ExpressionNumericConverter.TryGetConstantIntValue(c.SemanticModel, binaryRight, out var right))
                    {
                        CheckExpression(c, binary, binaryLeft, right, Language.Syntax.ComparisonKind(binary));
                    }
                },
                Language.SyntaxKind.ComparisonKinds);

        protected void CheckExpression(SyntaxNodeAnalysisContext context, SyntaxNode issue, SyntaxNode expression, int constant, ComparisonKind comparison)
        {
            var result = comparison.Compare(constant);
            if (result.IsInvalid()
                && CollectionSizeSymbol(Language.Syntax.RemoveConditionalAcesss(expression), context.SemanticModel) is { } symbol
                && GetDeclaringTypeName(symbol) is { } symbolType)
            {
                context.ReportIssue(Diagnostic.Create(rule, issue.GetLocation(), symbol.Name, symbolType, result == CountComparisonResult.AlwaysTrue));
            }
        }

        private ISymbol CollectionSizeSymbol(SyntaxNode expression, SemanticModel semanticModel) =>
            HasCandidateName(Language.Syntax.NodeIdentifier(expression)?.ValueText)
            && semanticModel.GetSymbolInfo(expression).Symbol is ISymbol symbol
            && IsCollecionSizeCheck(symbol)
            ? symbol : null;

        private bool HasCandidateName(string name) =>
            CountName.Equals(name, Language.NameComparison)
            || LengthName.Equals(name, Language.NameComparison)
            || LongLengthName.Equals(name, Language.NameComparison);

        private bool IsCollecionSizeCheck(ISymbol symbol) =>
            IsEnumerableCountMethod(symbol)
            || IsArrayLengthProperty(symbol)
            || IsStringLengthProperty(symbol)
            || IsCollectionProperty(symbol)
            || IsReadonlyCollectionProperty(symbol);

        private bool IsEnumerableCountMethod(ISymbol symbol) =>
            CountName.Equals(symbol.Name, Language.NameComparison)
            && symbol is IMethodSymbol methodSymbol
            && methodSymbol.IsExtensionMethod
            && methodSymbol.ReceiverType != null
            && methodSymbol.IsExtensionOn(KnownType.System_Collections_Generic_IEnumerable_T);

        private bool IsArrayLengthProperty(ISymbol symbol) =>
            (LengthName.Equals(symbol.Name, Language.NameComparison) || LongLengthName.Equals(symbol.Name, Language.NameComparison))
            && symbol is IPropertySymbol propertySymbol
            && propertySymbol.ContainingType.Is(KnownType.System_Array);

        private bool IsStringLengthProperty(ISymbol symbol) =>
            LengthName.Equals(symbol.Name, Language.NameComparison)
            && symbol is IPropertySymbol propertySymbol
            && propertySymbol.ContainingType.Is(KnownType.System_String);

        private bool IsCollectionProperty(ISymbol symbol) =>
            CountName.Equals(symbol.Name, Language.NameComparison)
            && symbol is IPropertySymbol propertySymbol
            && propertySymbol.ContainingType.DerivesOrImplements(KnownType.System_Collections_Generic_ICollection_T);

        private bool IsReadonlyCollectionCountProperty(ISymbol symbol) =>
            CountName.Equals(symbol.Name, Language.NameComparison)
            && symbol is IPropertySymbol propertySymbol
            && propertySymbol.ContainingType.DerivesOrImplements(KnownType.System_Collections_Generic_IReadOnlyCollection_T);

        private string GetDeclaringTypeName(ISymbol symbol)
        {
            if (IsArrayLengthProperty(symbol))
            {
                return nameof(Array);
            }
            else if (IsStringLengthProperty(symbol))
            {
                return nameof(String);
            }
            else if (IsEnumerableCountMethod(symbol))
            {
                return IEnumerableTString;
            }
            else if (IsCollectionProperty(symbol))
            {
                return nameof(ICollection<object>);
            }
            else if (IsReadonlyCollectionProperty(symbol))
            {
                return nameof(IReadOnlyCollection<object>);
            }
            else
            {
                return null;
            }
        }
    }
}
