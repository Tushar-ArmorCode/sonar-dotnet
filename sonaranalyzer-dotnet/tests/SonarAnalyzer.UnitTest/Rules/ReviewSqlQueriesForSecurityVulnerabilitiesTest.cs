﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2018 SonarSource SA
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

extern alias csharp;
using System.Data.SqlServerCe;
using System.IO;
using System.Linq;
using csharp::SonarAnalyzer.Rules.CSharp;
using FluentAssertions;
using Google.Protobuf;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Oracle.ManagedDataAccess.Client;
using SonarAnalyzer.Common;
using SonarAnalyzer.Protobuf.Ucfg;

namespace SonarAnalyzer.UnitTest.Rules
{
    [TestClass]
    public class ReviewSqlQueriesForSecurityVulnerabilitiesTest
    {
        internal static readonly MetadataReference OracleManagedDataAccessClientAssembly =
            MetadataReference.CreateFromFile(typeof(OracleCommand).Assembly.Location);
        internal static readonly MetadataReference SystemDataSqlServerCeAssembly =
            MetadataReference.CreateFromFile(typeof(SqlCeCommand).Assembly.Location);

        public TestContext TestContext { get; set; }

        [TestMethod]
        [TestCategory("Rule")]
        public void ReviewSqlQueriesForSecurityVulnerabilities()
        {
            Verifier.VerifyAnalyzer(@"TestCases\ReviewSqlQueriesForSecurityVulnerabilities.cs",
                new ReviewSqlQueriesForSecurityVulnerabilities(),
                null,
                Verifier.SystemDataAssembly, OracleManagedDataAccessClientAssembly, SystemDataSqlServerCeAssembly);
        }

        [TestMethod]
        [TestCategory("Rule")]
        public void ReviewSqlQueriesForSecurityVulnerabilities_GenerateProtobuf()
        {
            var workDir = Path.Combine(TestContext.TestRunResultsDirectory, "0");
            Directory.CreateDirectory(workDir);

            Verifier.VerifyCSharpAnalyzer(@"
class Program
{
    string Property2
    {
        set { var x = value; }
        get { return ""; }
    }
    void Method1(string s) { }
    string Method2() => null;
    string Property1 => null;
    public Program(string s) { }
    ~Program() { } // not generated, cannot accept tainted input
    public static string operator +(Program left, string right)
    {
        return null;
    }
    public event System.EventHandler Event
    {
        add { } // not generated, cannot accept tainted input
        remove { } // not generated, cannot accept tainted input
    }
}
",
                new ReviewSqlQueriesForSecurityVulnerabilities(new TestAnalyzerConfiguration(workDir)));

            var ucfgPath = Path.Combine(TestContext.TestRunResultsDirectory, "ucfg_cs");
            Directory.Exists(ucfgPath).Should().BeTrue();
            Directory.GetFiles(ucfgPath).Select(Path.GetFileName).Should().BeEquivalentTo(
                new[]
                {
                    "ucfg_0_1.pb",
                    "ucfg_0_2.pb",
                    "ucfg_0_3.pb",
                    "ucfg_0_4.pb",
                    "ucfg_0_5.pb",
                    "ucfg_0_6.pb",
                    "ucfg_0_7.pb",
                });
            Directory.GetFiles(ucfgPath).Select(GetProtobufMethodId).Should().BeEquivalentTo(
                new[]
                {
                    "foo.dll;Program.get_Property1()",
                    "foo.dll;Program.get_Property2()",
                    "foo.dll;Program.set_Property2(mscorlib;string)",
                    "foo.dll;Program..ctor(mscorlib;string)",
                    "foo.dll;Program.op_Addition(foo.dll;Program,mscorlib;string)",
                    "foo.dll;Program.Method1(mscorlib;string)",
                    "foo.dll;Program.Method2()",
                });
        }

        private static string GetProtobufMethodId(string protobufPath)
        {
            File.Exists(protobufPath).Should().BeTrue($"File {protobufPath} must exist.");
            using (var stream = File.OpenRead(protobufPath))
            {
                var ucfg = new UCFG();
                ucfg.MergeFrom(stream);
                return ucfg.MethodId;
            }
        }

        private class TestAnalyzerConfiguration : IAnalyzerConfiguration
        {
            private readonly string workDir;

            public TestAnalyzerConfiguration(string workDir)
            {
                this.workDir = workDir;
            }

            public string GetProjectOutputPath(AnalyzerOptions options)
            {
                return workDir;
            }
        }
    }
}
