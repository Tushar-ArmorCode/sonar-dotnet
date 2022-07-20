/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2022 SonarSource SA
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

using SonarAnalyzer.Rules.CSharp;

namespace SonarAnalyzer.UnitTest.Rules
{
    [TestClass]
    public class UnusedReturnValueTest
    {
        private readonly VerifierBuilder builder = new VerifierBuilder<UnusedReturnValue>();

        [Ignore][TestMethod]
        public void UnusedReturnValue() =>
            builder.AddPaths("UnusedReturnValue.cs").WithOptions(ParseOptionsHelper.FromCSharp8).Verify();

        [Ignore][TestMethod]
        public void UnusedReturnValueWithPartialClasses() =>
            builder.AddPaths("UnusedReturnValue.part1.cs", "UnusedReturnValue.part2.cs", "UnusedReturnValue.External.cs").WithOptions(ParseOptionsHelper.FromCSharp8).Verify();

#if NET

        [Ignore][TestMethod]
        public void UnusedReturnValue_CSharp9() =>
            builder.AddPaths("UnusedReturnValue.CSharp9.cs").WithTopLevelStatements().Verify();

        [Ignore][TestMethod]
        public void UnusedReturnValue_CSharp10() =>
            builder.AddPaths("UnusedReturnValue.CSharp10.cs").WithTopLevelStatements().WithOptions(ParseOptionsHelper.FromCSharp10).Verify();

        [Ignore][TestMethod]
        public void UnusedReturnValue_CSharpPreview() =>
            builder.AddPaths("UnusedReturnValue.CSharpPreview.cs").WithOptions(ParseOptionsHelper.CSharpPreview).Verify();

#endif

    }
}
