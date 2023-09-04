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
using SonarAnalyzer.Protobuf;
using SonarAnalyzer.Rules.CSharp;
using SonarAnalyzer.UnitTest.Helpers;

namespace SonarAnalyzer.UnitTest.Rules
{
    [TestClass]
    public class MetricsAnalyzerTest
    {
        private const string BasePath = @"Utilities\MetricsAnalyzer\";
        private const string AllMetricsFileName = "AllMetrics.cs";
        private const string RazorFileName = "Razor.razor";

        public TestContext TestContext { get; set; }

        [DataTestMethod]
        public void VerifyMetrics() =>
            CreateBuilder(false, AllMetricsFileName)
                .WithSonarProjectConfigPath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
                .VerifyUtilityAnalyzer<MetricsInfo>(messages =>
                    {
                        messages.Should().ContainSingle();
                        var metrics = messages.Single();
                        metrics.FilePath.Should().Be(Path.Combine(BasePath, AllMetricsFileName));
                        metrics.ClassCount.Should().Be(4);
                        metrics.CodeLine.Should().HaveCount(24);
                        metrics.CognitiveComplexity.Should().Be(1);
                        metrics.Complexity.Should().Be(2);
                        metrics.ExecutableLines.Should().HaveCount(5);
                        metrics.FunctionCount.Should().Be(1);
                        metrics.NoSonarComment.Should().HaveCount(1);
                        metrics.NonBlankComment.Should().HaveCount(1);
                        metrics.StatementCount.Should().Be(5);
                    });
#if NET

        [TestMethod]
        public void VerifyMetrics_Razor()
        {
            using var scope = new EnvironmentVariableScope(false) { EnableRazorAnalysis = true };

            CreateBuilder(false, RazorFileName)
                .WithSonarProjectConfigPath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
                .VerifyUtilityAnalyzer<MetricsInfo>(messages =>
                {
                    messages.Should().HaveCount(2); // Razor.razor and _Imports.razor

                    var metrics = messages.Single(x => x.FilePath.EndsWith(RazorFileName));

                    // ToDo: other metrics will be fixed in subsequent PRs.
                    metrics.ClassCount.Should().Be(1);
                    metrics.CodeLine.Should().BeEquivalentTo(new[] { 3, 5, 11, 13, 14, 15, 17, 20, 21, 22, 24, 26, 27, 30, 31, 32, 34, 35, 37, 38, 8 });
                    metrics.CognitiveComplexity.Should().Be(3);
                    metrics.Complexity.Should().Be(4);
                    metrics.ExecutableLines.Should().BeEquivalentTo(new[] { 3, 5, 11, 13, 15, 22, 26, 27, 30, 34, 37 });
                    metrics.FunctionCount.Should().Be(2);
                    metrics.NoSonarComment.Should().BeEmpty();
                    metrics.NonBlankComment.Should().BeEquivalentTo(new[] {13, 20, 21, 26, 27, 30, 31, 34, 35, 36, 8});
                    metrics.StatementCount.Should().Be(31);
                });
        }

#endif

        [TestMethod]
        public void Verify_NotRunForTestProject() =>
            CreateBuilder(true, AllMetricsFileName).VerifyUtilityAnalyzerProducesEmptyProtobuf();

        [DataTestMethod]
        [DataRow(AllMetricsFileName, true)]
        [DataRow("SomethingElse.cs", false)]
        public void Verify_UnchangedFiles(string unchangedFileName, bool expectedProtobufIsEmpty)
        {
            var builder = CreateBuilder(false, AllMetricsFileName)
                .WithSonarProjectConfigPath(AnalysisScaffolding.CreateSonarProjectConfigWithUnchangedFiles(TestContext, BasePath + unchangedFileName));
            if (expectedProtobufIsEmpty)
            {
                builder.VerifyUtilityAnalyzerProducesEmptyProtobuf();
            }
            else
            {
                builder.VerifyUtilityAnalyzer<TokenTypeInfo>(x => x.Should().NotBeEmpty());
            }
        }

        private VerifierBuilder CreateBuilder(bool isTestProject, string fileName)
        {
            var testRoot = BasePath + TestContext.TestName;
            return new VerifierBuilder()
                .AddAnalyzer(() => new TestMetricsAnalyzer(testRoot, isTestProject))
                .AddPaths(fileName)
                .WithBasePath(BasePath)
                .WithOptions(ParseOptionsHelper.CSharpLatest)
                .WithProtobufPath(@$"{testRoot}\metrics.pb");
        }

        // We need to set protected properties and this class exists just to enable the analyzer without bothering with additional files with parameters
        private sealed class TestMetricsAnalyzer : MetricsAnalyzer
        {
            public TestMetricsAnalyzer(string outPath, bool isTestProject)
            {
                IsAnalyzerEnabled = true;
                OutPath = outPath;
                IsTestProject = isTestProject;
            }
        }
    }
}
