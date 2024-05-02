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

using CS = SonarAnalyzer.Rules.CSharp;
using VB = SonarAnalyzer.Rules.VisualBasic;

namespace SonarAnalyzer.Test.Rules
{
    [TestClass]
    public class IfCollapsibleTest
    {

#if NETFRAMEWORK

        public static IEnumerable<MetadataReference> AspNet4xReferences(string aspNetMvcVersion) =>
            MetadataReferenceFacade.SystemWeb                                          // For HttpAttributeMethod and derived attributes
            .Concat(NuGetMetadataReference.MicrosoftAspNetMvc(aspNetMvcVersion));      // For Controller

        [TestMethod]
        public void FrameworkViewCompiler_CS() =>
            new VerifierBuilder<CS.FrameworkViewCompiler>()
            .AddReferences(AspNet4xReferences("5.2.7"))
            .AddSnippet("""
                @{
                    var uselessInView = 42;
                }
                <main> </main>
                """,
                "Contact.cshtml")
            .AddSnippet("""
                public class Thingy
                {
                    void DoWork()
                    {
                        var uselessInNormalFile = 42;
                    }
                }
                """,
                "Thingy.cs")
            .Verify();
#endif

        [TestMethod]
        public void IfCollapsible_CS() =>
            new VerifierBuilder<CS.IfCollapsible>().AddPaths("IfCollapsible.cs").Verify();

        [TestMethod]
        public void IfCollapsible_VB() =>
            new VerifierBuilder<VB.IfCollapsible>().AddPaths("IfCollapsible.vb").Verify();
    }
}
