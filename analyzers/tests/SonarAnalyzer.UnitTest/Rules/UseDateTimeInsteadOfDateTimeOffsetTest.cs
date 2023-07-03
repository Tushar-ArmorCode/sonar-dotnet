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

namespace SonarAnalyzer.UnitTest.Rules;

[TestClass]
public class UseDateTimeInsteadOfDateTimeOffsetTest
{
    private readonly VerifierBuilder builderCS = new VerifierBuilder<CS.UseDateTimeInsteadOfDateTimeOffset>();
    private readonly VerifierBuilder builderVB = new VerifierBuilder<VB.UseDateTimeInsteadOfDateTimeOffset>();

    [TestMethod]
    public void UseDateTimeInsteadOfDateTimeOffset_CS() =>
        builderCS.AddPaths("UseDateTimeInsteadOfDateTimeOffset.cs").Verify();

#if NET

    [TestMethod]
    public void UseDateTimeInsteadOfDateTimeOffset_CSharp9() =>
        builderCS.AddPaths("UseDateTimeInsteadOfDateTimeOffset.CSharp9.cs").WithTopLevelStatements().Verify();

    [TestMethod]
    public void UseDateTimeInsteadOfDateTimeOffset_VB_Net() =>
        builderVB.AddPaths("UseDateTimeInsteadOfDateTimeOffset.Net.vb").Verify();

#endif

    [TestMethod]
    public void UseDateTimeInsteadOfDateTimeOffset_VB() =>
        builderVB.AddPaths("UseDateTimeInsteadOfDateTimeOffset.vb").Verify();
}
