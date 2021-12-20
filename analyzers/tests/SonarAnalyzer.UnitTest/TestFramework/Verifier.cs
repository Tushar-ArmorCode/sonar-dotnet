﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2021 SonarSource SA
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

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using SonarAnalyzer.Common;
using SonarAnalyzer.UnitTest.Helpers;

namespace SonarAnalyzer.UnitTest.TestFramework
{
    internal class Verifier
    {
        private readonly VerifierBuilder builder;

        public Verifier(VerifierBuilder builder)
        {
            this.builder = builder ?? throw new ArgumentNullException(nameof(builder));
            // FIXME: Validate input
        }

        public void Verify()    // This should never has any arguments
        {
            using var scope = new EnvironmentVariableScope { EnableConcurrentAnalysis = true };     // ToDo: Implement properly
            var paths = builder.Paths.Select(x => Path.GetFullPath(Path.Combine("TestCases", x)));
            var pathsWithConcurrencyTests = paths.Count() == 1 ? CreateConcurrencyTest(paths) : paths;  // FIXME: Redesign
            var solution = SolutionBuilder.CreateSolutionFromPaths(pathsWithConcurrencyTests, OutputKind.DynamicallyLinkedLibrary, builder.References);
            var analyzers = builder.Analyzers.Select(x => x()).ToArray();
            foreach (var compilation in solution.Compile(builder.ParseOptions.ToArray()))
            {
                DiagnosticVerifier.Verify(compilation, analyzers, CompilationErrorBehavior.Default, null, null);
            }
        }

        private static IEnumerable<string> CreateConcurrencyTest(IEnumerable<string> paths)
        {
            var language = AnalyzerLanguage.FromPath(paths.First());    // FIXME: Redesign
            foreach (var path in paths)
            {
                var newPath = Path.ChangeExtension(path, ".Concurrent." + language.FileExtension);  // FIXME: dot
                var content = File.ReadAllText(path, Encoding.UTF8);
                File.WriteAllText(newPath, InsertConcurrentNamespace(content, language));
                yield return newPath;
            }
        }

        private static string InsertConcurrentNamespace(string content, AnalyzerLanguage language)   // FIXME: Not static
        {
            return language.LanguageName switch
            {
                LanguageNames.CSharp => $"namespace AppendedNamespaceForConcurrencyTest {{ {content} }}",
                LanguageNames.VisualBasic => content.Insert(ImportsIndexVB(), "Namespace AppendedNamespaceForConcurrencyTest : ") + " : End Namespace",
                _ => throw language.ToUnexpectedLanguageException()
            };

            int ImportsIndexVB() =>
                Regex.Match(content, @"^\s*Imports\s+.+$", RegexOptions.Multiline | RegexOptions.RightToLeft) is { Success: true } match ? match.Index + match.Length + 1 : 0;
        }
    }
}
