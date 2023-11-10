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

#if NET

using SonarAnalyzer.Rules.CSharp;

namespace SonarAnalyzer.UnitTest.Rules;

[TestClass]
public class SupplyParameterFromQueryNeedsRoutableComponentTest
{
    private readonly VerifierBuilder builder = new VerifierBuilder<SupplyParameterFromQueryNeedsRoutableComponent>();
    public TestContext TestContext { get; set; }

    [TestMethod]
    public void SupplyParameterFromQueryNeedsRoutableComponent_Blazor() =>
        builder.AddPaths("SupplyParameterFromQueryNeedsRoutableComponent_Compliant.razor",
                         "SupplyParameterFromQueryNeedsRoutableComponent_Noncompliant.razor")
               .WithAdditionalFilePath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
               .Verify();

    [TestMethod]
    public void SupplyParameterFromQueryNeedsRoutableComponent_Partial() =>
        builder.AddPaths("SupplyParameterFromQueryNeedsRoutableComponent_Partial.cs",
                         "SupplyParameterFromQueryNeedsRoutableComponent_Partial.razor")
               .WithAdditionalFilePath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
               .Verify();

    [TestMethod]
    public void SupplyParameterFromQueryNeedRoutableComponent_CS() =>
        builder.AddPaths("SupplyParameterFromQueryNeedsRoutableComponent_Compliant.cs",
                         "SupplyParameterFromQueryNeedsRoutableComponent_Noncompliant.cs")
               .AddReferences(NuGetMetadataReference.MicrosoftAspNetCoreComponents("7.0.13"))
               .Verify();

    [TestMethod]
    public void BlazorQueryParameterTypeShouldBeSupported_Blazor() =>
        builder
            .AddPaths("BlazorQueryParameterTypeShouldBeSupported.razor")
            .WithAdditionalFilePath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
            .Verify();

    [TestMethod]
    public void BlazorQueryParameterTypeShouldBeSupported_BlazorNoRoute() =>
        builder
            .AddPaths("BlazorQueryParameterTypeShouldBeSupported.NoRoute.razor")
            .WithAdditionalFilePath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
            .VerifyNoIssueReported();

    [TestMethod]
    public void BlazorQueryParameterTypeShouldBeSupported_Partial() =>
        builder
            .AddPaths("BlazorQueryParameterTypeShouldBeSupported.Partial.razor", "BlazorQueryParameterTypeShouldBeSupported.Partial.razor.cs")
            .WithAdditionalFilePath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
            .Verify();

    [TestMethod]
    public void BlazorQueryParameterTypeShouldBeSupported_CS() =>
        builder
            .AddPaths("BlazorQueryParameterTypeShouldBeSupported.cs")
            .AddReferences(NuGetMetadataReference.MicrosoftAspNetCoreComponents("7.0.13"))
            .Verify();
}

#endif
