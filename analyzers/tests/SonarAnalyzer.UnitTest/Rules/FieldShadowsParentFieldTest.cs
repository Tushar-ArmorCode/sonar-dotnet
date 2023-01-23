﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2023 SonarSource SA
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
    public class FieldShadowsParentFieldTest
    {
        private readonly VerifierBuilder builderCS = new VerifierBuilder<CS.FieldShadowsParentField>();
        private readonly VerifierBuilder builderVB = new VerifierBuilder<VB.FieldShadowsParentField>();

        [TestMethod]
        public void FieldShadowsParentField_CS() =>
            builderCS.AddPaths("FieldShadowsParentField.cs").Verify();

        [TestMethod]
        public void FieldShadowsParentField_VB() =>
            builderVB.AddPaths("FieldShadowsParentField.vb").Verify();

        [TestMethod]
        public void FieldShadowsParentField_DoesNotRaiseIssuesForTestProject_CS() =>
            builderCS.AddPaths("FieldShadowsParentField.cs")
                .AddTestReference()
                .VerifyNoIssueReported();

        [TestMethod]
        public void FieldShadowsParentField_DoesNotRaiseIssuesForTestProject_VB() =>
            builderVB.AddPaths("FieldShadowsParentField.vb")
                .AddTestReference()
                .VerifyNoIssueReported();

#if NET

        [TestMethod]
        public void FieldShadowsParentField_CSharp9() =>
            builderCS.AddPaths("FieldShadowsParentField.CSharp9.cs")
                .WithOptions(ParseOptionsHelper.FromCSharp9)
                .Verify();

        [TestMethod]
        public void FieldsShouldNotDifferByCapitalization_CShar9() =>
            builderCS.AddPaths("FieldsShouldNotDifferByCapitalization.CSharp9.cs")
                .WithOptions(ParseOptionsHelper.FromCSharp9)
                .Verify();

#endif

        [DataTestMethod]
        [DataRow(ProjectType.Product)]
        [DataRow(ProjectType.Test)]
        public void FieldsShouldNotDifferByCapitalization_CS(ProjectType projectType) =>
            builderCS.AddPaths("FieldsShouldNotDifferByCapitalization.cs")
                .AddReferences(TestHelper.ProjectTypeReference(projectType))
                .Verify();

        [DataTestMethod]
        [DataRow(ProjectType.Product)]
        [DataRow(ProjectType.Test)]
        public void FieldsShouldNotDifferByCapitalization_VB(ProjectType projectType) =>
            builderVB.AddPaths("FieldsShouldNotDifferByCapitalization.vb")
                .AddReferences(TestHelper.ProjectTypeReference(projectType))
                .Verify();
    }
}
