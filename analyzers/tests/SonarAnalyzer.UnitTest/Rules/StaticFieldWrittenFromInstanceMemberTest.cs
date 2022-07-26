﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2022 SonarSource SA
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

using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp;
using SonarAnalyzer.Rules.CSharp;

namespace SonarAnalyzer.UnitTest.Rules
{
    [TestClass]
    public class StaticFieldWrittenFromInstanceMemberTest
    {
        private readonly VerifierBuilder<StaticFieldWrittenFromInstanceMember> builder = new();

        [TestMethod]
        public void StaticFieldWrittenFromInstanceMember() =>
            builder.AddPaths(@"StaticFieldWrittenFromInstanceMember.cs").WithOptions(ParseOptionsHelper.FromCSharp8).AddReferences(MetadataReferenceFacade.NETStandard21).Verify();

        [TestMethod]
        public async Task SecondaryIssueInReferencedCompilation()
        {
            const string firstClass =
                @"
public class Foo
{
    public static int Count = 0; // Secondary
}
";

            const string secondClass =
                @"
public class Bar
{
    public int Increment() => Foo.Count++;
}
";

            var analyzers = ImmutableArray<DiagnosticAnalyzer>.Empty.Add(new StaticFieldWrittenFromInstanceMember());
            var firstCompilation = CreateCompilation(CSharpSyntaxTree.ParseText(firstClass), "First").WithAnalyzers(analyzers).Compilation;
            var secondCompilation = CreateCompilation(CSharpSyntaxTree.ParseText(secondClass), "Second")
                                    .AddReferences(firstCompilation.ToMetadataReference())
                                    .WithAnalyzers(analyzers);

            var result = await secondCompilation.GetAnalyzerDiagnosticsAsync();
            result.Single().GetMessage().Should().StartWith("Analyzer 'SonarAnalyzer.Rules.CSharp.StaticFieldWrittenFromInstanceMember' threw an exception of type 'System.ArgumentException' with message 'Reported diagnostic 'S2696' has a source location in file '', which is not part of the compilation being analyzed.");
        }

#if NET
        [TestMethod]
        public void StaticFieldWrittenFromInstanceMember_CSharp9() =>
            builder.AddPaths(@"StaticFieldWrittenFromInstanceMember.CSharp9.cs").WithTopLevelStatements().Verify();

        [TestMethod]
        public void StaticFieldWrittenFromInstanceMember_CSharp10() =>
            builder.AddPaths(@"StaticFieldWrittenFromInstanceMember.CSharp10.cs").WithTopLevelStatements().WithOptions(ParseOptionsHelper.FromCSharp10).Verify();
#endif

        private static CSharpCompilation CreateCompilation(SyntaxTree tree, string name) =>
            CSharpCompilation
                .Create(name, options: new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddReferences(MetadataReference.CreateFromFile(typeof(string).Assembly.Location))
                .AddSyntaxTrees(tree);
    }
}
