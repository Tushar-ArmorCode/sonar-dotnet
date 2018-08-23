﻿// Copyright (c) Tunnel Vision Laboratories, LLC. All Rights Reserved.
// Licensed under the Apache License, Version 2.0. See LICENSE in the project root for license information.

namespace SonarAnalyzer.ShimLayer.CSharp
{
    using System;
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;

    internal struct IsPatternExpressionSyntaxWrapper : ISyntaxWrapper<ExpressionSyntax>
    {
        internal const string WrappedTypeName = "Microsoft.CodeAnalysis.CSharp.Syntax.IsPatternExpressionSyntax";
        private static readonly Type WrappedType;

        private static readonly Func<ExpressionSyntax, ExpressionSyntax> ExpressionAccessor;
        private static readonly Func<ExpressionSyntax, SyntaxToken> IsKeywordAccessor;
        private static readonly Func<ExpressionSyntax, CSharpSyntaxNode> PatternAccessor;
        private static readonly Func<ExpressionSyntax, ExpressionSyntax, ExpressionSyntax> WithExpressionAccessor;
        private static readonly Func<ExpressionSyntax, SyntaxToken, ExpressionSyntax> WithIsKeywordAccessor;
        private static readonly Func<ExpressionSyntax, CSharpSyntaxNode, ExpressionSyntax> WithPatternAccessor;

        static IsPatternExpressionSyntaxWrapper()
        {
            WrappedType = WrapperHelper.GetWrappedType(typeof(IsPatternExpressionSyntaxWrapper));
            ExpressionAccessor = LightupHelpers.CreateSyntaxPropertyAccessor<ExpressionSyntax, ExpressionSyntax>(WrappedType, nameof(Expression));
            IsKeywordAccessor = LightupHelpers.CreateSyntaxPropertyAccessor<ExpressionSyntax, SyntaxToken>(WrappedType, nameof(IsKeyword));
            PatternAccessor = LightupHelpers.CreateSyntaxPropertyAccessor<ExpressionSyntax, CSharpSyntaxNode>(WrappedType, nameof(Pattern));
            WithExpressionAccessor = LightupHelpers.CreateSyntaxWithPropertyAccessor<ExpressionSyntax, ExpressionSyntax>(WrappedType, nameof(Expression));
            WithIsKeywordAccessor = LightupHelpers.CreateSyntaxWithPropertyAccessor<ExpressionSyntax, SyntaxToken>(WrappedType, nameof(IsKeyword));
            WithPatternAccessor = LightupHelpers.CreateSyntaxWithPropertyAccessor<ExpressionSyntax, CSharpSyntaxNode>(WrappedType, nameof(Pattern));
        }

        private IsPatternExpressionSyntaxWrapper(ExpressionSyntax node)
        {
            SyntaxNode = node;
        }

        public ExpressionSyntax SyntaxNode { get; }

        public ExpressionSyntax Expression
        {
            get
            {
                return ExpressionAccessor(SyntaxNode);
            }
        }

        public SyntaxToken IsKeyword
        {
            get
            {
                return IsKeywordAccessor(SyntaxNode);
            }
        }

        public PatternSyntaxWrapper Pattern
        {
            get
            {
                return (PatternSyntaxWrapper)PatternAccessor(SyntaxNode);
            }
        }

        public static explicit operator IsPatternExpressionSyntaxWrapper(SyntaxNode node)
        {
            if (node == null)
            {
                return default(IsPatternExpressionSyntaxWrapper);
            }

            if (!IsInstance(node))
            {
                throw new InvalidCastException($"Cannot cast '{node.GetType().FullName}' to '{WrappedTypeName}'");
            }

            return new IsPatternExpressionSyntaxWrapper((ExpressionSyntax)node);
        }

        public static implicit operator ExpressionSyntax(IsPatternExpressionSyntaxWrapper wrapper)
        {
            return wrapper.SyntaxNode;
        }

        public static bool IsInstance(SyntaxNode node)
        {
            return node != null && LightupHelpers.CanWrapNode(node, WrappedType);
        }

        public IsPatternExpressionSyntaxWrapper WithExpression(ExpressionSyntax expression)
        {
            return new IsPatternExpressionSyntaxWrapper(WithExpressionAccessor(SyntaxNode, expression));
        }

        public IsPatternExpressionSyntaxWrapper WithIsKeyword(SyntaxToken isKeyword)
        {
            return new IsPatternExpressionSyntaxWrapper(WithIsKeywordAccessor(SyntaxNode, isKeyword));
        }

        public IsPatternExpressionSyntaxWrapper WithPattern(PatternSyntaxWrapper pattern)
        {
            return new IsPatternExpressionSyntaxWrapper(WithPatternAccessor(SyntaxNode, pattern.SyntaxNode));
        }
    }
}
