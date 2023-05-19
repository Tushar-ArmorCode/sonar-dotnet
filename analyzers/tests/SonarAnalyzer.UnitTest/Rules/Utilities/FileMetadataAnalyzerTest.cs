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

using System.IO;
using Moq;
using SonarAnalyzer.AnalysisContext;
using SonarAnalyzer.Protobuf;
using SonarAnalyzer.Rules;
using SonarAnalyzer.Rules.CSharp;

namespace SonarAnalyzer.UnitTest.Rules
{
    [TestClass]
    public class FileMetadataAnalyzerTest
    {
        private const string BasePath = @"Utilities\FileMetadataAnalyzer\";

        public TestContext TestContext { get; set; }

        [DataTestMethod]
        [DataRow(ProjectType.Product)]
        [DataRow(ProjectType.Test)]
        public void Autogenerated(ProjectType projectType)
        {
            var autogeneratedFiles = new[]
            {
                "ExtraEmptyFile.g.cs", // This is added by Verifier, we will skip it when building the project
                "autogenerated_comment.cs",
                "autogenerated_comment2.cs",
                "class.designer.cs",
                "class.g.cs",
                "class.g.something.cs",
                "class.generated.cs",
                "class_generated.cs",
                "compiler_generated.cs",
                "compiler_generated_attr.cs",
                "debugger_non_user_code.cs",
                "debugger_non_user_code_attr.cs",
                "generated_code_attr.cs",
                "generated_code_attr_local_function.cs",
                "generated_code_attr2.cs",
                "TEMPORARYGENERATEDFILE_class.cs"
            };
            var projectFiles = autogeneratedFiles
                .Skip(1) // "ExtraEmptyFile.g.cs" is automatically added by Verifier
                .ToArray();

            VerifyAllFilesAreGenerated(projectType, projectFiles, autogeneratedFiles);
        }

        [DataTestMethod]
        [DataRow(ProjectType.Product)]
        [DataRow(ProjectType.Test)]
        public void NotAutogenerated(ProjectType projectType)
        {
            var notAutogeneratedFiles = new[]
            {
                "normal_file.cs",
                "generated_region.cs",
                "generated_region_2.cs"
            };
            CreateBuilder(projectType, notAutogeneratedFiles)
                .WithAdditionalFilePath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, projectType))
                .VerifyUtilityAnalyzer<FileMetadataInfo>(messages =>
                    messages.Should().BeEquivalentTo(notAutogeneratedFiles.Select(expected => new FileMetadataInfo
                    {
                        IsGenerated = false,
                        FilePath = BasePath + expected,
                    }).Append(new FileMetadataInfo
                    {
                        IsGenerated = true,
                        FilePath = "ExtraEmptyFile.g.cs"
                    })));
        }

        [DataTestMethod]
        [DataRow(true)]
        [DataRow(false)]
        public void CreateMessage_NoEncoding_SetsEmptyString(bool isTestProject)
        {
            var tree = new Mock<SyntaxTree>();
            tree.SetupGet(x => x.FilePath).Returns("File.Generated.cs");    // Generated to simplify mocking for GeneratedCodeRecognizer
            tree.SetupGet(x => x.Encoding).Returns(() => null);
            var sut = new TestFileMetadataAnalyzer(null, isTestProject);

            sut.TestCreateMessage(UtilityAnalyzerParameters.Default, tree.Object, null).Encoding.Should().BeEmpty();
        }

        [DataTestMethod]
        [DataRow("class.generated.cs", 0)]
        [DataRow("SomethingElse.cs", 1)]
        public void Verify_UnchangedFiles(string unchangedFileName, int expectedFileCount) =>
            CreateBuilder(ProjectType.Product, "class.generated.cs")
                .WithAdditionalFilePath(AnalysisScaffolding.CreateSonarProjectConfigWithUnchangedFiles(TestContext, BasePath + unchangedFileName))
                .VerifyUtilityAnalyzer<FileMetadataInfo>(x => x.Should().HaveCount(expectedFileCount + 1)); // +1 to ignore ExtraEmptyFile.g.cs

#if NET

        [DataTestMethod]
        [DataRow("Razor.razor", "EmptyProject.GlobalUsings.g.cs", ".NETCoreApp,Version=v7.0.AssemblyAttributes.cs", "EmptyProject.AssemblyInfo.cs")]
        [DataRow("Razor.cshtml", "EmptyProject.GlobalUsings.g.cs", ".NETCoreApp,Version=v7.0.AssemblyAttributes.cs", "EmptyProject.AssemblyInfo.cs", "EmptyProject.RazorAssemblyInfo.cs")]
        public void Verify_RazorFilesAreIgnored(string fileName, params string[] expectedFiles) =>
            CreateBuilder(ProjectType.Product, fileName)
                .WithAdditionalFilePath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
                .VerifyUtilityAnalyzer<FileMetadataInfo>(messages =>
                    messages.Select(x => Path.GetFileName(x.FilePath)).Should().BeEquivalentTo(expectedFiles));

#endif

        private void VerifyAllFilesAreGenerated(ProjectType projectType, string[] projectFiles, string[] autogeneratedFiles) =>
            CreateBuilder(projectType, projectFiles)
                .WithAdditionalFilePath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, projectType))
                .VerifyUtilityAnalyzer<FileMetadataInfo>(messages =>
                {
                    messages.Should().AllBeEquivalentTo(new { IsGenerated = true });
                    messages.Should().SatisfyRespectively(autogeneratedFiles.Select<string, Action<FileMetadataInfo>>(expected => actual => actual.FilePath.EndsWith(expected)));
                });

        private VerifierBuilder CreateBuilder(ProjectType projectType, params string[] projectFiles)
        {
            var testRoot = BasePath + TestContext.TestName;
            return new VerifierBuilder()
                .AddAnalyzer(() => new TestFileMetadataAnalyzer(testRoot, projectType == ProjectType.Test))
                .AddPaths(projectFiles)
                .WithBasePath(BasePath)
                .WithOptions(ParseOptionsHelper.CSharpLatest)
                .WithProtobufPath(@$"{testRoot}\file-metadata.pb");
        }

        // We need to set protected properties and this class exists just to enable the analyzer without bothering with additional files with parameters
        private sealed class TestFileMetadataAnalyzer : FileMetadataAnalyzer
        {
            private readonly string outPath;
            private readonly bool isTestProject;

            public TestFileMetadataAnalyzer(string outPath, bool isTestProject)
            {
                this.outPath = outPath;
                this.isTestProject = isTestProject;
            }

            protected override UtilityAnalyzerParameters ReadParameters(SonarCompilationStartAnalysisContext context) =>
                base.ReadParameters(context) with { IsAnalyzerEnabled = true, OutPath = outPath, IsTestProject = isTestProject };

            public FileMetadataInfo TestCreateMessage(UtilityAnalyzerParameters parameters, SyntaxTree syntaxTree, SemanticModel semanticModel) =>
                CreateMessage(parameters, syntaxTree, semanticModel);
        }
    }
}
