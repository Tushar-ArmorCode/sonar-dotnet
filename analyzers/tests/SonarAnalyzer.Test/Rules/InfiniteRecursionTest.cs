﻿/*
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

using SonarAnalyzer.Rules.CSharp;

namespace SonarAnalyzer.Test.Rules
{
    [TestClass]
    public class InfiniteRecursionTest
    {
        private readonly VerifierBuilder sonarCfg = new VerifierBuilder()
            .AddAnalyzer(() => new InfiniteRecursion(AnalyzerConfiguration.AlwaysEnabledWithSonarCfg))
            .AddReferences(MetadataReferenceFacade.NETStandard21);

        private readonly VerifierBuilder roslynCfg = new VerifierBuilder<InfiniteRecursion>()
            .AddReferences(MetadataReferenceFacade.NETStandard21);

        [TestMethod]
        public void InfiniteRecursion_SonarCfg() =>
            sonarCfg.AddPaths("InfiniteRecursion.SonarCfg.cs")
                .WithOptions(ParseOptionsHelper.OnlyCSharp7)
                .Verify();

        [TestMethod]
        public void InfiniteRecursion_RoslynCfg() =>
            roslynCfg.AddPaths("InfiniteRecursion.RoslynCfg.cs")
                .WithOptions(ParseOptionsHelper.FromCSharp8)
                .Verify();

#if NET

        [TestMethod]
        public void InfiniteRecursion_RoslynCfg_CSharp9() =>
            roslynCfg.AddPaths("InfiniteRecursion.RoslynCfg.CSharp9.cs")
                .WithOptions(ParseOptionsHelper.FromCSharp9)
                .Verify();

        [TestMethod]
        public void InfiniteRecursion_CSharp11() =>
            roslynCfg.AddPaths("InfiniteRecursion.CSharp11.cs")
                .WithOptions(ParseOptionsHelper.FromCSharp11)
                .Verify();

#endif

    }
}
