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

namespace SonarAnalyzer.Test.Rules;

#if NET

[TestClass]
public class ControllersReuseClientTest
{
    private readonly VerifierBuilder builder = new VerifierBuilder<ControllersReuseClient>()
        .WithBasePath("AspNet")
        .AddReferences(
        [
            AspNetCoreMetadataReference.MicrosoftAspNetCoreMvcAbstractions,
            AspNetCoreMetadataReference.MicrosoftAspNetCoreMvcCore,
            AspNetCoreMetadataReference.MicrosoftAspNetCoreMvcViewFeatures,
            .. MetadataReferenceFacade.SystemThreadingTasks,
            .. NuGetMetadataReference.SystemNetHttp(),
            .. NuGetMetadataReference.MicrosoftExtensionsHttp()
        ]);

    [TestMethod]
    public void ControllersReuseClient_CS() =>
        builder
            .AddPaths("ControllersReuseClient.cs")
            .Verify();

    [TestMethod]
    public void ControllersReuseClient_CS8() =>
        builder
            .AddPaths("ControllersReuseClient.CSharp8.cs")
            .WithOptions(ParseOptionsHelper.FromCSharp8)
            .Verify();

    [TestMethod]
    public void ControllersReuseClient_CS9() =>
        builder
            .AddPaths("ControllersReuseClient.CSharp9.cs")
            .WithOptions(ParseOptionsHelper.FromCSharp9)
            .Verify();

    [TestMethod]
    public void ControllersReuseClient_CS12() =>
        builder
            .AddPaths("ControllerReuseClient.CSharp12.cs")
            .WithOptions(ParseOptionsHelper.FromCSharp12).Verify();
}
#endif
