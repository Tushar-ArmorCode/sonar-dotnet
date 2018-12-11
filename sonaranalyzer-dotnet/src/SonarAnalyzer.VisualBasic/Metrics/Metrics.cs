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

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.VisualBasic;
using Microsoft.CodeAnalysis.VisualBasic.Syntax;

namespace SonarAnalyzer.Common.VisualBasic
{
    public sealed class Metrics : MetricsBase
    {
        private readonly Lazy<ImmutableArray<SyntaxNode>> publicApis;

        public Metrics(SyntaxTree tree)
            : base(tree)
        {
            var root = tree.GetRoot();
            if (root.Language != LanguageNames.VisualBasic)
            {
                throw new ArgumentException(InitalizationErrorTextPattern, nameof(tree));
            }

            publicApis = new Lazy<ImmutableArray<SyntaxNode>>(GetPublicApiFactory(tree.GetRoot()));
        }

        protected override bool IsEndOfFile(SyntaxToken token) =>
            token.IsKind(SyntaxKind.EndOfFileToken);

        protected override bool IsNoneToken(SyntaxToken token) =>
            token.IsKind(SyntaxKind.None);

        protected override bool IsCommentTrivia(SyntaxTrivia trivia)
        {
            switch (trivia.Kind())
            {
                case SyntaxKind.CommentTrivia:
                case SyntaxKind.DocumentationCommentExteriorTrivia:
                case SyntaxKind.DocumentationCommentTrivia:
                    return true;

                default:
                    return false;
            }
        }

        protected override bool IsClass(SyntaxNode node)
        {
            switch (node.Kind())
            {
                case SyntaxKind.ClassBlock:
                case SyntaxKind.StructureBlock:
                case SyntaxKind.InterfaceBlock:
                case SyntaxKind.ModuleBlock:
                    return true;

                default:
                    return false;
            }
        }

        protected override bool IsStatement(SyntaxNode node) =>
            node is ExecutableStatementSyntax;

        protected override bool IsFunction(SyntaxNode node)
        {
            if (!FunctionKinds.Contains(node.Kind()) ||
                !MethodBlocks.Contains(node.Parent.Kind()) ||
                node.Parent.Parent.IsKind(SyntaxKind.InterfaceBlock))
            {
                return false;
            }

            if (node is MethodBaseSyntax method && method.Modifiers.Any(SyntaxKind.MustOverrideKeyword))
            {
                return false;
            }

            return true;
        }

        protected override IEnumerable<SyntaxNode> PublicApiNodes =>
            publicApis.Value;

        private static Func<ImmutableArray<SyntaxNode>> GetPublicApiFactory(SyntaxNode root) =>
            () =>
            {
                var walker = new PublicApiWalker();
                walker.Visit(root);
                return walker.PublicApi;
            };

        private bool IsComplexityIncreasingKind(SyntaxNode node)
        {
            switch (node.Kind())
            {
                case SyntaxKind.IfStatement:
                case SyntaxKind.SingleLineIfStatement:
                case SyntaxKind.TernaryConditionalExpression:
                case SyntaxKind.CaseStatement:

                case SyntaxKind.WhileStatement:
                case SyntaxKind.DoWhileStatement:
                case SyntaxKind.DoUntilStatement:
                case SyntaxKind.SimpleDoStatement:
                case SyntaxKind.ForStatement:
                case SyntaxKind.ForEachStatement:

                case SyntaxKind.ThrowStatement:
                case SyntaxKind.TryStatement:

                case SyntaxKind.ErrorStatement:

                case SyntaxKind.ResumeStatement:
                case SyntaxKind.ResumeNextStatement:
                case SyntaxKind.ResumeLabelStatement:

                case SyntaxKind.OnErrorGoToLabelStatement:
                case SyntaxKind.OnErrorGoToMinusOneStatement:
                case SyntaxKind.OnErrorGoToZeroStatement:
                case SyntaxKind.OnErrorResumeNextStatement:

                case SyntaxKind.GoToStatement:

                case SyntaxKind.ExitDoStatement:
                case SyntaxKind.ExitForStatement:
                case SyntaxKind.ExitFunctionStatement:
                case SyntaxKind.ExitOperatorStatement:
                case SyntaxKind.ExitPropertyStatement:
                case SyntaxKind.ExitSelectStatement:
                case SyntaxKind.ExitSubStatement:
                case SyntaxKind.ExitTryStatement:
                case SyntaxKind.ExitWhileStatement:

                case SyntaxKind.ContinueDoStatement:
                case SyntaxKind.ContinueForStatement:
                case SyntaxKind.ContinueWhileStatement:

                case SyntaxKind.StopStatement:

                case SyntaxKind.AndAlsoExpression:
                case SyntaxKind.OrElseExpression:

                case SyntaxKind.EndStatement:
                    return true;

                default:
                    return false;
            }
        }

        public override int GetComplexity(SyntaxNode node) =>
            node.DescendantNodesAndSelf()
                .Count(n =>
                    IsComplexityIncreasingKind(n) ||
                    IsFunction(n));

        public override int GetCognitiveComplexity(SyntaxNode node)
        {
            return 0; // Not implemented
        }

        public override ICollection<int> ExecutableLines =>
            new List<int>(); // Not implemented

        private static readonly ISet<SyntaxKind> FunctionKinds = new HashSet<SyntaxKind>
        {
            SyntaxKind.SubNewStatement,
            SyntaxKind.SubStatement,
            SyntaxKind.FunctionStatement,
            SyntaxKind.OperatorStatement,
            SyntaxKind.GetAccessorStatement,
            SyntaxKind.SetAccessorStatement,
            SyntaxKind.RaiseEventAccessorStatement,
            SyntaxKind.AddHandlerAccessorStatement,
            SyntaxKind.RemoveHandlerAccessorStatement,
            SyntaxKind.DeclareSubStatement,
            SyntaxKind.DeclareFunctionStatement
        };

        private static readonly ISet<SyntaxKind> MethodBlocks = new HashSet<SyntaxKind>
        {
            SyntaxKind.ConstructorBlock,
            SyntaxKind.FunctionBlock,
            SyntaxKind.SubBlock,
            SyntaxKind.OperatorBlock,
            SyntaxKind.GetAccessorBlock,
            SyntaxKind.SetAccessorBlock,
            SyntaxKind.RaiseEventAccessorBlock,
            SyntaxKind.AddHandlerAccessorBlock,
            SyntaxKind.RemoveHandlerAccessorBlock
        };
    }
}
