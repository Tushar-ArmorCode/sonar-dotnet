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

using Moq;
using SonarAnalyzer.Common;
using static SonarAnalyzer.Helpers.KnownReference;

namespace SonarAnalyzer.UnitTest;

[TestClass]
public class KnownReferenceTests
{
    [TestMethod]
    public void KnownReference_ThrowsWhenPredicateNull_1()
    {
        var sut = () => new KnownReference(default(Func<AssemblyIdentity, bool>));
        sut.Should().Throw<ArgumentNullException>().Which.ParamName.Should().Be("predicate");
    }

    [TestMethod]
    public void KnownReference_ThrowsWhenPredicateNull_2()
    {
        var sut = () => new KnownReference(default(Func<IEnumerable<AssemblyIdentity>, bool>));
        sut.Should().Throw<ArgumentNullException>().Which.ParamName.Should().Be("predicate");
    }

    [TestMethod]
    public void KnownReference_ThrowsWhenPredicateNull_Params()
    {
        var sut = () => new KnownReference(_ => true, null, _ => true);
        sut.Should().Throw<ArgumentException>().Which.ParamName.Should().Be("or");
    }

    [DataTestMethod]
    [DataRow("Test", true)]
    [DataRow("test", false)]
    [DataRow("TEST", false)]
    [DataRow("MyTest", false)]
    [DataRow("TestMy", false)]
    [DataRow("MyTestMy", false)]
    [DataRow("MyTESTMy", false)]
    [DataRow("Without", false)]
    public void NameIs_Test(string name, bool expected)
    {
        var sut = new KnownReference(NameIs("Test"));
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity(name);
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [DataTestMethod]
    [DataRow("Test", true)]
    [DataRow("test", false)]
    [DataRow("TEST", false)]
    [DataRow("MyTest", true)]
    [DataRow("TestMy", true)]
    [DataRow("MyTestMy", true)]
    [DataRow("MyTESTMy", false)]
    [DataRow("Without", false)]
    public void NameContains_Test(string name, bool expected)
    {
        var sut = new KnownReference(Contains("Test"));
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity(name);
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [DataTestMethod]
    [DataRow("Test", true)]
    [DataRow("test", false)]
    [DataRow("TEST", false)]
    [DataRow("MyTest", false)]
    [DataRow("TestMy", true)]
    [DataRow("MyTestMy", false)]
    [DataRow("MyTESTMy", false)]
    [DataRow("Without", false)]
    public void NameStartsWith_Test(string name, bool expected)
    {
        var sut = new KnownReference(StartsWith("Test"));
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity(name);
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [DataTestMethod]
    [DataRow("Test", true)]
    [DataRow("test", false)]
    [DataRow("TEST", false)]
    [DataRow("MyTest", true)]
    [DataRow("TestMy", false)]
    [DataRow("MyTestMy", false)]
    [DataRow("MyTESTMy", false)]
    [DataRow("Without", false)]
    public void NameEndsWith_Test(string name, bool expected)
    {
        var sut = new KnownReference(EndsWith("Test"));
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity(name);
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [DataTestMethod]
    [DataRow("1.0.0.0", false)]
    [DataRow("1.9.9.99", false)]
    [DataRow("2.0.0.0", true)]
    [DataRow("2.0.0.1", true)]
    [DataRow("2.1.0.0", true)]
    [DataRow("3.1.0.0", true)]
    public void Version_GreaterOrEqual_2_0(string version, bool expected)
    {
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity("assemblyName", new Version(version));
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        var sut = new KnownReference(VersionGreaterOrEqual(new Version(2, 0)));
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
        sut = new KnownReference(VersionGreaterOrEqual("2.0"));
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [DataTestMethod]
    [DataRow("1.0.0.0", true)]
    [DataRow("1.9.9.99", true)]
    [DataRow("2.0.0.0", false)]
    [DataRow("2.0.0.1", false)]
    [DataRow("2.1.0.0", false)]
    [DataRow("3.1.0.0", false)]
    public void Version_LowerThen_2_0(string version, bool expected)
    {
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity("assemblyName", new Version(version));
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        var sut = new KnownReference(VersionLowerThen(new Version(2, 0)));
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
        sut = new KnownReference(VersionLowerThen("2.0"));
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [DataTestMethod]
    [DataRow("1.0.0.0", false)]
    [DataRow("1.9.9.99", false)]
    [DataRow("2.0.0.0", true)]
    [DataRow("2.0.0.1", true)]
    [DataRow("2.1.0.0", true)]
    [DataRow("3.1.0.0", true)]
    [DataRow("3.4.9.99", true)]
    [DataRow("3.5.0.0", true)]
    [DataRow("3.5.0.1", false)]
    [DataRow("10.0.0.0", false)]
    public void Version_Between_2_0_and_3_5(string version, bool expected)
    {
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity("assemblyName", new Version(version));
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        var sut = new KnownReference(VersionBetween(new Version(2, 0, 0, 0), new Version(3, 5, 0, 0)));
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
        sut = new KnownReference(VersionBetween("2.0.0.0", "3.5.0.0"));
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [DataTestMethod]
    [DataRow("c5b62af9de6d7244", true)]
    [DataRow("C5B62AF9DE6D7244", true)]
    [DataRow("c5-b6-2a-f9-de-6d-72-44", true)]
    [DataRow(
        "002400000480000094000000060200000024000052534131000400000100010081b4345a022cc0f4b42bdc795a5a7a1623c1e58dc2246645d751ad41ba98f2749dc5c4e0da3a9e09febcb2cd5b088a0f" +
        "041f8ac24b20e736d8ae523061733782f9c4cd75b44f17a63714aced0b29a59cd1ce58d8e10ccdb6012c7098c39871043b7241ac4ab9f6b34f183db716082cd57c1ff648135bece256357ba735e67dc6", true)]
    [DataRow("AA-68-91-16-d3-a4-ae-33", false)]
    public void PublicKeyTokenIs_c5b62af9de6d7244(string publicKeyToken, bool expected)
    {
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity("assemblyName", publicKeyOrToken: ImmutableArray.Create<byte>(
            0x00, 0x24, 0x00, 0x00, 0x04, 0x80, 0x00, 0x00, 0x94, 0x00, 0x00, 0x00, 0x06, 0x02, 0x00, 0x00, 0x00, 0x24, 0x00, 0x00, 0x52, 0x53, 0x41, 0x31, 0x00,
            0x04, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x81, 0xb4, 0x34, 0x5a, 0x02, 0x2c, 0xc0, 0xf4, 0xb4, 0x2b, 0xdc, 0x79, 0x5a, 0x5a, 0x7a, 0x16, 0x23, 0xc1,
            0xe5, 0x8d, 0xc2, 0x24, 0x66, 0x45, 0xd7, 0x51, 0xad, 0x41, 0xba, 0x98, 0xf2, 0x74, 0x9d, 0xc5, 0xc4, 0xe0, 0xda, 0x3a, 0x9e, 0x09, 0xfe, 0xbc, 0xb2,
            0xcd, 0x5b, 0x08, 0x8a, 0x0f, 0x04, 0x1f, 0x8a, 0xc2, 0x4b, 0x20, 0xe7, 0x36, 0xd8, 0xae, 0x52, 0x30, 0x61, 0x73, 0x37, 0x82, 0xf9, 0xc4, 0xcd, 0x75,
            0xb4, 0x4f, 0x17, 0xa6, 0x37, 0x14, 0xac, 0xed, 0x0b, 0x29, 0xa5, 0x9c, 0xd1, 0xce, 0x58, 0xd8, 0xe1, 0x0c, 0xcd, 0xb6, 0x01, 0x2c, 0x70, 0x98, 0xc3,
            0x98, 0x71, 0x04, 0x3b, 0x72, 0x41, 0xac, 0x4a, 0xb9, 0xf6, 0xb3, 0x4f, 0x18, 0x3d, 0xb7, 0x16, 0x08, 0x2c, 0xd5, 0x7c, 0x1f, 0xf6, 0x48, 0x13, 0x5b,
            0xec, 0xe2, 0x56, 0x35, 0x7b, 0xa7, 0x35, 0xe6, 0x7d, 0xc6), hasPublicKey: true);
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        var sut = new KnownReference(PublicKeyTokenIs(publicKeyToken));
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
        sut = new KnownReference(OptionalPublicKeyTokenIs(publicKeyToken));
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [TestMethod]
    public void PublicKeyTokenIs_FailsWhenKeyIsMissing()
    {
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity("assemblyName", hasPublicKey: false);
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        var sut = new KnownReference(PublicKeyTokenIs("c5b62af9de6d7244"));
        sut.IsReferencedBy(compilation.Object).Should().BeFalse();
    }

    [TestMethod]
    public void OptionalPublicKeyTokenIs_SucceedsWhenKeyIsMissing()
    {
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity("assemblyName", hasPublicKey: false);
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        var sut = new KnownReference(OptionalPublicKeyTokenIs("c5b62af9de6d7244"));
        sut.IsReferencedBy(compilation.Object).Should().BeTrue();
    }

    [DataTestMethod]
    [DataRow("Test", "1.0.0.0", false)]
    [DataRow("Test", "1.9.9.99", false)]
    [DataRow("TestMy", "2.0.0.0", true)]
    [DataRow("MyTest", "2.0.0.0", false)]
    [DataRow("TestMy", "3.5.0.0", true)]
    [DataRow("TestMy", "3.5.0.1", false)]
    [DataRow("Test", "10.0.0.0", false)]
    public void Combinator_NameStartWith_Test_And_Version_Between_2_0_And_3_5(string name, string version, bool expected)
    {
        var sut = new KnownReference(StartsWith("Test").And(VersionBetween(new Version(2, 0, 0, 0), new Version(3, 5, 0, 0))));
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity(name, new Version(version));
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [DataTestMethod]
    [DataRow("Start", true)]
    [DataRow("End", true)]
    [DataRow("StartOrEnd", true)]
    [DataRow("StartTest", true)]
    [DataRow("TestEnd", true)]
    [DataRow("EndStart", false)]
    [DataRow("EndSomething", false)]
    [DataRow("SomethingStart", false)]
    public void Combinator_StartsWith_Start_Or_EndsWith_End(string name, bool expected)
    {
        var sut = new KnownReference(StartsWith("Start"), EndsWith("End"));
        var compilation = new Mock<Compilation>("compilationName", ImmutableArray<MetadataReference>.Empty, new Dictionary<string, string>(), false, null, null);
        var identity = new AssemblyIdentity(name);
        compilation.SetupGet(x => x.ReferencedAssemblyNames).Returns(new[] { identity });
        sut.IsReferencedBy(compilation.Object).Should().Be(expected);
    }

    [TestMethod]
    public void XUnitAssert_2_4()
    {
        var compilation = SolutionBuilder.Create()
            .AddProject(AnalyzerLanguage.CSharp)
            .AddReferences(NuGetMetadataReference.XunitFramework("2.4.2"))
            .AddSnippet("// Empty file")
            .GetCompilation();
        compilation.References(XUnit_Assert).Should().BeTrue();
    }

    [TestMethod]
    public void XUnitAssert_1_9()
    {
        var compilation = SolutionBuilder.Create()
            .AddProject(AnalyzerLanguage.CSharp)
            .AddReferences(NuGetMetadataReference.XunitFrameworkV1)
            .AddSnippet("// Empty file")
            .GetCompilation();
        compilation.References(XUnit_Assert).Should().BeTrue();
    }

    [TestMethod]
    public void XUnitAssert_NoReference()
    {
        var compilation = SolutionBuilder.Create()
            .AddProject(AnalyzerLanguage.CSharp)
            .AddSnippet("// Empty file")
            .GetCompilation();
        compilation.References(XUnit_Assert).Should().BeFalse();
    }
}
