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

using Microsoft.VisualStudio.TestTools.UnitTesting;
using SonarAnalyzer.Common;
using System;
using FluentAssertions;

namespace SonarAnalyzer.UnitTest.Common
{
    [TestClass]
    public class AnalyzerLanguageTest
    {
        [TestMethod]
        public void AnalyzerLanguage_Parse()
        {
            var parsed = AnalyzerLanguage.Parse("cs");
            parsed.Should().Be(AnalyzerLanguage.CSharp);
        }

        [TestMethod]
        [ExpectedException(typeof(NotSupportedException))]
        public void AnalyzerLanguage_Parse_Fail()
        {
            var parsed = AnalyzerLanguage.Parse("csharp");
            parsed.Should().Be(AnalyzerLanguage.CSharp);
        }

        [TestMethod]
        public void AnalyzerLanguage_GetDirectory()
        {
            AnalyzerLanguage.CSharp.GetDirectoryName()
                .Should().Be("CSharp");
            AnalyzerLanguage.VisualBasic.GetDirectoryName()
                .Should().Be("VisualBasic");
        }

        [TestMethod]
        public void AnalyzerLanguage_GetQualityProfileRepositoryKey()
        {
            AnalyzerLanguage.CSharp.GetQualityProfileRepositoryKey()
                .Should().Be("csharpsquid");
            AnalyzerLanguage.VisualBasic.GetQualityProfileRepositoryKey()
                .Should().Be("vbnet");
        }

        [TestMethod]
        public void AnalyzerLanguage_Operations()
        {
            AnalyzerLanguage.CSharp.AddLanguage(AnalyzerLanguage.VisualBasic)
                .Should().Be(AnalyzerLanguage.Both);
            AnalyzerLanguage.CSharp.AddLanguage(AnalyzerLanguage.CSharp)
                .Should().Be(AnalyzerLanguage.CSharp);
            AnalyzerLanguage.CSharp.AddLanguage(AnalyzerLanguage.Both)
                .Should().Be(AnalyzerLanguage.Both);

            AnalyzerLanguage.CSharp.IsAlso(AnalyzerLanguage.CSharp)
                .Should().BeTrue();
            AnalyzerLanguage.CSharp.IsAlso(AnalyzerLanguage.VisualBasic)
                .Should().BeFalse();
            AnalyzerLanguage.Both.IsAlso(AnalyzerLanguage.VisualBasic)
                .Should().BeTrue();
        }
    }
}
