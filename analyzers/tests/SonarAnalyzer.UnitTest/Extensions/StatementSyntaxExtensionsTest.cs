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

using System.Linq;
using FluentAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using SonarAnalyzer.Extensions;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.UnitTest.Extensions
{
    [TestClass]
    public class StatementSyntaxExtensionsTest
    {
        private const string Source = @"
namespace Test
{
    class TestClass
    {
        int a = 1;
        public void DoSomething(){}
        public void IfMethod()
        {
            if (a>1)
                DoSomething();
            if (a<0)
                DoSomething();
        }
    }
}";

        private const string SourceTopLevelStatement = @"
var a = 1;
var b = 2;
if (a == b)
{
    DoSomething();
}
void DoSomething() { }";

        [TestMethod]
        public void GetPrecedingStatement()
        {
            var syntaxTree = CSharpSyntaxTree.ParseText(Source);
            var ifMethod = syntaxTree.GetRoot()
                                     .DescendantNodes()
                                     .OfType<MethodDeclarationSyntax>()
                                     .First(m => m.Identifier.ValueText == "IfMethod");

            var statements = ifMethod.Body.Statements.ToList();

            statements[1].GetPrecedingStatement().Should().BeEquivalentTo(statements[0]);
            statements[0].GetPrecedingStatement().Should().Be(null);
        }

        [TestMethod]
        public void GetPrecedingStatementTopLevelStatements()
        {
            var syntaxTreeTopLevelStatement = CSharpSyntaxTree.ParseText(SourceTopLevelStatement);
            var variableDeclarators = syntaxTreeTopLevelStatement.GetRoot()
                                                                 .DescendantNodes()
                                                                 .OfType<LocalDeclarationStatementSyntax>()
                                                                 .ToArray();
            var aDeclaration = variableDeclarators[0];
            var bDeclaration = variableDeclarators[1];
            aDeclaration.GetPrecedingStatement().Should().Be(null);
            bDeclaration.GetPrecedingStatement().Should().BeEquivalentTo(aDeclaration);
        }
    }
}
