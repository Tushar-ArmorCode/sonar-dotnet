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

using System.Collections.Generic;
using System.Linq;
using FluentAssertions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using SonarAnalyzer.Rules.CSharp;
using SonarAnalyzer.UnitTest.MetadataReferences;
using SonarAnalyzer.UnitTest.TestFramework;

namespace SonarAnalyzer.UnitTest.Rules
{
    [TestClass]
    public class UnnecessaryUsingsTest
    {
        [TestMethod]
        [TestCategory("Rule")]
        public void UnnecessaryUsings() =>
            Verifier.VerifyAnalyzerNoDuplication(new[] { @"TestCases\UnnecessaryUsings.cs", @"TestCases\UnnecessaryUsings2.cs" },
                                                           new UnnecessaryUsings(),
                                                           additionalReferences: GetAdditionalReferences());

#if NET

        [TestMethod]
        [TestCategory("Rule")]
        public void UnnecessaryUsings_CSharp9() =>
            Verifier.VerifyNonConcurrentAnalyzerFromCSharp9Console(@"TestCases\UnnecessaryUsings.CSharp9.cs", new UnnecessaryUsings());

        [TestMethod]
        [TestCategory("Rule")]
        public void UnnecessaryUsings_TupleDeconstruct_NetCore() =>
            Verifier.VerifyNonConcurrentAnalyzer(@"TestCases\UnnecessaryUsings.TupleDeconstruct.NetCore.cs", new UnnecessaryUsings());

#elif NETFRAMEWORK

        [TestMethod]
        [TestCategory("Rule")]
        public void UnnecessaryUsings_TupleDeconstruct_NetFx() =>
            Verifier.VerifyNonConcurrentAnalyzer(@"TestCases\UnnecessaryUsings.TupleDeconstruct.NetFx.cs", new UnnecessaryUsings());

#endif

        [TestMethod]
        [TestCategory("CodeFix")]
        public void UnnecessaryUsings_CodeFix() =>
            Verifier.VerifyCodeFix(@"TestCases\UnnecessaryUsings.cs",
                                   @"TestCases\UnnecessaryUsings.Fixed.cs",
                                   @"TestCases\UnnecessaryUsings.Fixed.Batch.cs",
                                   new UnnecessaryUsings(),
                                   new UnnecessaryUsingsCodeFixProvider(),
                                   additionalReferences: GetAdditionalReferences());

        [TestMethod]
        public void EquivalentNameSyntax_Equals_Object()
        {
            var main = new EquivalentNameSyntax(SyntaxFactory.IdentifierName("Lorem"));
            object same = new EquivalentNameSyntax(SyntaxFactory.IdentifierName("Lorem"));
            object different = new EquivalentNameSyntax(SyntaxFactory.IdentifierName("Ipsum"));

            main.Equals(same).Should().BeTrue();
            main.Equals(null).Should().BeFalse();
            main.Equals("different type").Should().BeFalse();
            main.Equals(different).Should().BeFalse();
        }

        [TestMethod]
        public void EquivalentNameSyntax_Equals_EquivalentNameSyntax()
        {
            var main = new EquivalentNameSyntax(SyntaxFactory.IdentifierName("Lorem"));
            var same = new EquivalentNameSyntax(SyntaxFactory.IdentifierName("Lorem"));
            var different = new EquivalentNameSyntax(SyntaxFactory.IdentifierName("Ipsum"));

            main.Equals(same).Should().BeTrue();
            main.Equals(null).Should().BeFalse();
            main.Equals(different).Should().BeFalse();
        }

        private static IEnumerable<MetadataReference> GetAdditionalReferences() =>
            MetadataReferenceFacade.MicrosoftWin32Primitives.Concat(MetadataReferenceFacade.SystemSecurityCryptography);
    }
}
