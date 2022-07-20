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

using CS = SonarAnalyzer.Rules.CSharp;
using VB = SonarAnalyzer.Rules.VisualBasic;

namespace SonarAnalyzer.UnitTest.Rules
{
    [TestClass]
    public class ConditionalStructureSameImplementationTest
    {
        private readonly VerifierBuilder csBuilder = new VerifierBuilder<CS.ConditionalStructureSameImplementation>();
        private readonly VerifierBuilder vbBuilder = new VerifierBuilder<VB.ConditionalStructureSameImplementation>();

        [Ignore][TestMethod]
        public void ConditionalStructureSameImplementation_If_CSharp() =>
            csBuilder.AddPaths("ConditionalStructureSameImplementation_If.cs").Verify();

        [Ignore][TestMethod]
        public void ConditionalStructureSameImplementation_Switch_CSharp() =>
            csBuilder.AddPaths("ConditionalStructureSameImplementation_Switch.cs").Verify();

        [Ignore][TestMethod]
        public void ConditionalStructureSameImplementation_If_VisualBasic() =>
            vbBuilder.AddPaths("ConditionalStructureSameImplementation_If.vb").Verify();

        [Ignore][TestMethod]
        public void ConditionalStructureSameImplementation_Switch_VisualBasic() =>
            vbBuilder.AddPaths("ConditionalStructureSameImplementation_Switch.vb").Verify();
    }
}
