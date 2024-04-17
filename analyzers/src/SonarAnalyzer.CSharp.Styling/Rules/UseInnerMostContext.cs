/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2024 SonarSource SA
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

namespace SonarAnalyzer.Rules.CSharp.Styling;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class UseInnerMostContext : StylingAnalyzer
{
    public UseInnerMostContext() : base("T0005", "Use the inner-most '{0}' context.") { }

    protected override void Initialize(SonarAnalysisContext context) =>
        context.RegisterNodeAction(c =>
        {
            var memberAccess = (MemberAccessExpressionSyntax)c.Node;
            if (memberAccess.Name.GetName().StartsWith("Register")
                && IsParentLambdaParameter(memberAccess, memberAccess.Expression.GetName()))
            {
                c.ReportIssue(Rule, memberAccess.Expression, memberAccess.Expression.GetName());
            }
        }, SyntaxKind.SimpleMemberAccessExpression);

    private bool IsParentLambdaParameter(SyntaxNode node, string name)
    {
        node = node.FirstAncestorOrSelf<SyntaxNode>(x => x is SimpleLambdaExpressionSyntax)?.Parent;
        while (node != null)
        {
            if (node is SimpleLambdaExpressionSyntax lambda && lambda.Parameter.GetName().Equals(name, StringComparison.Ordinal))
            {
                return true;
            }

            node = node.Parent;
        }

        return false;
    }
}
