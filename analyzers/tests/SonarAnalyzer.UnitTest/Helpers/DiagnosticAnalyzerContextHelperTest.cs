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

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;
using CS = SonarAnalyzer.Rules.CSharp;
using VB = SonarAnalyzer.Rules.VisualBasic;

namespace SonarAnalyzer.UnitTest.Helpers
{
    [TestClass]
    public class DiagnosticAnalyzerContextHelperTest
    {
        [TestMethod]
        public void No_Issue_On_Generated_File_With_Generated_Name()
        {
            const string sourceCs =
@"namespace Generated
{
    class MyClass
    {
        void M()
        {
            ;;;;
        }
    }
}";
            VerifyEmpty("test.g.cs", sourceCs, new CS.EmptyStatement());

            const string sourceVb =
@"Module Module1
    Sub Main()
        Dim foo() As String ' Noncompliant
    End Sub
End Module";
            VerifyEmpty("test.g.vb", sourceVb, new VB.ArrayDesignatorOnVariable());
        }

        [TestMethod]
        public void No_Issue_On_Generated_File_With_AutoGeneratedComment()
        {
            var sourceCs =
@"// ------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     Runtime Version:2.0.50727.3053
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
// ------------------------------------------------------------------------------
namespace Generated
{
    class MyClass
    {
        void M()
        {
            ;;;;
        }
    }
}";
            VerifyEmpty("test.cs", sourceCs, new CS.EmptyStatement());

            sourceCs =
@"// <autogenerated />
namespace Generated
{
    class MyClass
    {
        void M()
        {
            ;;;;
        }
    }
}";
            VerifyEmpty("test.cs", sourceCs, new CS.EmptyStatement());

            sourceCs =
@"/*
 * No description provided (generated by Swagger Codegen https://github.com/swagger-api/swagger-codegen)
 *
 * OpenAPI spec version: 1.0.0
 *
 * Generated by: https://github.com/swagger-api/swagger-codegen.git
 */";
            VerifyEmpty("test.cs", sourceCs, new CS.EmptyStatement());

            sourceCs = "// Generated by the protocol buffer compiler.  DO NOT EDIT!";
            VerifyEmpty("test.cs", sourceCs, new CS.EmptyStatement());

            const string sourceVb =
@"'------------------------------------------------------------------------------
' <auto-generated>
'     This code was generated by a tool.
'     Runtime Version:2.0.50727.4927
'
'     Changes to this file may cause incorrect behavior and will be lost if
'     the code is regenerated.
' </auto-generated>
'------------------------------------------------------------------------------
Module Module1
    Sub Main()
        Dim foo() As String ' Noncompliant
    End Sub
End Module";
            VerifyEmpty("test.vb", sourceVb, new VB.ArrayDesignatorOnVariable());
        }

        [TestMethod]
        public void No_Issue_On_Generated_File_With_ExcludedAttribute()
        {
            const string sourceCs =
@"namespace Generated
{
    class MyClass
    {
        [System.Diagnostics.DebuggerNonUserCodeAttribute()]
        void M()
        {
            ;;;;
        }
    }
}";
            VerifyEmpty("test.cs", sourceCs, new CS.EmptyStatement());

            const string sourceVb =
@"Module Module1
    <System.Diagnostics.DebuggerNonUserCodeAttribute()>
    Sub Main()
        Dim foo() As String ' Noncompliant
    End Sub
End Module";
            VerifyEmpty("test.vb", sourceVb, new VB.ArrayDesignatorOnVariable());
        }

        [TestMethod]
        public void No_Issue_On_Generated_Lambda_With_ExcludedAttribute()
        {
            const string sourceCs =
                @"using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Net6Poc.GeneratedCodeRecognizer
{
    internal class TestCases
    {
        public void Bar()
        {
            [DebuggerNonUserCodeAttribute()] int Do() => 1;

            Action a = [CompilerGenerated] () => { ;;; };

            Action x = true
                           ? ([DebuggerNonUserCodeAttribute] () => { ;;; })
                           : [GenericAttribute<int>] () => { ;;; }; // FN? Empty statement in lambda

            Call([DebuggerNonUserCodeAttribute] (x) => { ;;; });
        }

        private void Call(Action<int> action) => action(1);
    }

    public class GenericAttribute<T> : Attribute { }
}
";
            VerifyEmpty("test.cs", sourceCs, new CS.EmptyStatement(), parseOptions: new CSharpParseOptions(LanguageVersion.Preview));
        }

        // until https://github.com/SonarSource/sonar-dotnet/issues/2228, we were considering a file as generated
        // if the word "generated" was contained inside a region.
        [DataTestMethod]
        [DataRow("generated stuff")]
        [DataRow("Contains FooGenerated methods")]
        [DataRow("Windows Form Designer generated code")] // legacy Windows Forms used to include generated code in dev files, surrounded by such a region
        [DataRow("Windows Form Designer GeNeRaTed code")] // legacy Windows Forms used to include generated code in dev files, surrounded by such a region
        public void Issues_Raised_On_Partially_Generated_Legacy_WinForms_File(string regionName)
        {
            var content =
$@"namespace PartiallyGenerated
{{
    class MyClass
    {{
        void HandWrittenEventHandler()
        {{
            ; // Noncompliant
        }}

#region {regionName}
        void GeneratedStuff()
        {{
            ; // Noncompliant
        }}
#endregion
    }}
}}";
            var compilation = SolutionBuilder
               .Create()
               .AddProject(AnalyzerLanguage.CSharp, createExtraEmptyFile: false)
               .AddSnippet(content, "Foo.cs")
               .GetCompilation();

            DiagnosticVerifier.Verify(compilation, new CS.EmptyStatement(), CompilationErrorBehavior.FailTest, compilation.SyntaxTrees.First());
        }

        [TestMethod]
        public void IsGenerated_On_GeneratedTree()
        {
            const string source =
@"namespace Generated
{
    class MyClass
    {
        [System.Diagnostics.DebuggerNonUserCodeAttribute()]
        void M()
        {
            ;;;;
        }
    }
}";

            var result = IsGenerated(source, CSharpGeneratedCodeRecognizer.Instance);
            result.Should().BeTrue();
        }

        [TestMethod]
        public void IsGenerated_On_GeneratedLocalFunctionTree()
        {
            const string source =
@"namespace Generated
{
    class MyClass
    {
        void M()
        {
            ;;;;
            [System.Diagnostics.DebuggerNonUserCodeAttribute()]
            void LocalFunction()
            {
                ;;;
            }
        }
    }
}";
            var result = IsGenerated(source, CSharpGeneratedCodeRecognizer.Instance);
            result.Should().BeTrue();
        }

        [TestMethod]
        public void IsGenerated_On_NonGeneratedTree()
        {
            const string source =
@"namespace NonGenerated
{
    class MyClass
    {
    }
}";

            var result = IsGenerated(source, CSharpGeneratedCodeRecognizer.Instance);
            result.Should().BeFalse();
        }

        private static bool IsGenerated(string content, GeneratedCodeRecognizer generatedCodeRecognizer)
        {
            var compilation = SolutionBuilder
               .Create()
               .AddProject(AnalyzerLanguage.CSharp, createExtraEmptyFile: false)
               .AddSnippet(content)
               .GetCompilation();

            var tree = compilation.SyntaxTrees.First();

            return tree.IsGenerated(generatedCodeRecognizer, compilation);
        }

        private static void VerifyEmpty(string name,
                                        string content,
                                        DiagnosticAnalyzer diagnosticAnalyzer,
                                        CompilationErrorBehavior checkMode = CompilationErrorBehavior.Default,
                                        ParseOptions parseOptions = default)
        {
            AnalyzerLanguage language;
            if (name.EndsWith(".cs"))
            {
                language = AnalyzerLanguage.CSharp;
            }
            else if (name.EndsWith(".vb"))
            {
                language = AnalyzerLanguage.VisualBasic;
            }
            else
            {
                throw new ArgumentException($"Was expecting the file name to end with '.cs' or '.vb', got '{name}'.", nameof(name));
            }

            var compilation = SolutionBuilder
               .Create()
               .AddProject(language, createExtraEmptyFile: false)
               .AddSnippet(content, name)
               .GetCompilation(parseOptions);

            DiagnosticVerifier.VerifyNoIssueReported(compilation, diagnosticAnalyzer, checkMode);
        }
    }
}
