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

using Microsoft.CodeAnalysis.CSharp;
using SonarAnalyzer.AnalysisContext;
using SonarAnalyzer.UnitTest.TestFramework.Tests;

namespace SonarAnalyzer.UnitTest.AnalysisContext;

[TestClass]
public class SonarSemanticModelReportingContextTest
{
    [TestMethod]
    public void Properties_ArePropagated()
    {
        var cancel = new CancellationToken(true);
        var (tree, model) = TestHelper.CompileCS("// Nothing to see here");
        var options = AnalysisScaffolding.CreateOptions();
        var context = new SemanticModelAnalysisContext(model, options, _ => { }, _ => true, cancel);
        var sut = new SonarSemanticModelReportingContext(AnalysisScaffolding.CreateSonarAnalysisContext(), context);

        sut.Tree.Should().BeSameAs(tree);
        sut.Compilation.Should().BeSameAs(model.Compilation);
        sut.SemanticModel.Should().BeSameAs(model);
        sut.Options.Should().BeSameAs(options);
        sut.Cancel.Should().Be(cancel);
    }

    [TestMethod]
    public void RegistrationIsExecuted_SonarAnalysisContext()
    {
        new VerifierBuilder().AddAnalyzer(() => new SemanticModelAnalyzer((context, g) =>
            context.RegisterSemanticModelAction(g, c =>
                c.ReportIssue(Diagnostic.Create(DummyAnalyzer.Rule, c.Tree.GetCompilationUnitRoot().GetFirstToken().GetLocation())))))
            .AddSnippet("""
            using System; // Noncompliant
            """)
            .Verify();
    }

    [TestMethod]
    public void RegistrationIsExecuted_SonarCompilationStartAnalysisContext()
    {
        new VerifierBuilder().AddAnalyzer(() => new SemanticModelAnalyzer((context, _) =>
            context.RegisterCompilationStartAction(start =>
                start.RegisterSemanticModelAction(c =>
                {
                    if (c.Tree.GetCompilationUnitRoot().GetFirstToken() is { RawKind: not (int)SyntaxKind.None } token)
                    {
                        c.ReportIssue(Diagnostic.Create(DummyAnalyzer.Rule, token.GetLocation()));
                    }
                }))))
            .AddSnippet("""
            using System; // Noncompliant
            """)
            .Verify();
    }

    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    internal class SemanticModelAnalyzer : DummyAnalyzer
    {
        protected override GeneratedCodeRecognizer GeneratedCodeRecognizer => CSharpGeneratedCodeRecognizer.Instance;

        public Action<SonarAnalysisContext, GeneratedCodeRecognizer> InitializeAction { get; }

        public SemanticModelAnalyzer(Action<SonarAnalysisContext, GeneratedCodeRecognizer> action) =>
            InitializeAction = action;

        protected override void Initialize(SonarAnalysisContext context) =>
            InitializeAction(context, GeneratedCodeRecognizer);
    }
}
