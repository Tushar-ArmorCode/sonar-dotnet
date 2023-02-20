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

using SonarAnalyzer.Rules.CSharp;
using SonarAnalyzer.UnitTest.MetadataReferences;

namespace SonarAnalyzer.UnitTest.Rules;

[TestClass]
public class AssertionsShouldBeCompleteTest
{
    private readonly VerifierBuilder fluentAssertions = new VerifierBuilder<AssertionsShouldBeComplete>()
        .AddReferences(NuGetMetadataReference.FluentAssertions("6.10"))
        .AddReferences(MetadataReferenceFacade.SystemXml)
        .AddReferences(MetadataReferenceFacade.SystemXmlLinq)
        .AddReferences(MetadataReferenceFacade.SystemNetHttp)
        .AddReferences(MetadataReferenceFacade.SystemData);

    [TestMethod]
    public void AssertionsShouldBeComplete_CSharp7() =>
        fluentAssertions
        .WithOptions(ParseOptionsHelper.OnlyCSharp7)
        .AddPaths("AssertionsShouldBeComplete.FluentAssertions.CSharp7.cs")
        .Verify();

    [TestMethod]
    public void AssertionsShouldBeComplete_CSharp8() =>
        fluentAssertions
        .WithOptions(ParseOptionsHelper.FromCSharp8)
        .AddPaths("AssertionsShouldBeComplete.FluentAssertions.CSharp8.cs")
        .WithConcurrentAnalysis(false)
        .Verify();
}
