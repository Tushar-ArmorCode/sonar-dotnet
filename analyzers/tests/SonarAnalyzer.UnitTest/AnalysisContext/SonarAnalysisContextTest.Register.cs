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

extern alias csharp;
extern alias vbnet;
using Microsoft.CodeAnalysis.CSharp;
using Moq;
using SonarAnalyzer.AnalysisContext;
using SonarAnalyzer.UnitTest.Helpers;
using SonarAnalyzer.UnitTest.TestFramework.Tests;
using CS = csharp::SonarAnalyzer.Extensions.SonarAnalysisContextExtensions;
using RoslynAnalysisContext = Microsoft.CodeAnalysis.Diagnostics.AnalysisContext;
using VB = vbnet::SonarAnalyzer.Extensions.SonarAnalysisContextExtensions;

namespace SonarAnalyzer.UnitTest.AnalysisContext;

public partial class SonarAnalysisContextTest
{
    private const string SnippetFileName = "snippet0.cs";
    private const string AnotherFileName = "Any other file name to make snippet0 considered as changed.cs";

    private static readonly ImmutableArray<DiagnosticDescriptor> DummyMainDescriptor = new[] { AnalysisScaffolding.CreateDescriptorMain() }.ToImmutableArray();

    [DataTestMethod]
    [DataRow(SnippetFileName, false)]
    [DataRow(AnotherFileName, true)]
    public void RegisterNodeAction_UnchangedFiles_SonarAnalysisContext(string unchangedFileName, bool expected)
    {
        var context = new DummyAnalysisContext(TestContext, unchangedFileName);
        var sut = new SonarAnalysisContext(context, DummyMainDescriptor);
        sut.RegisterNodeAction<SyntaxKind>(CSharpGeneratedCodeRecognizer.Instance, context.DelegateAction);

        context.AssertDelegateInvoked(expected);
    }

    [DataTestMethod]
    [DataRow(SnippetFileName, false)]
    [DataRow(AnotherFileName, true)]
    public void RegisterNodeAction_UnchangedFiles_SonarParametrizedAnalysisContext(string unchangedFileName, bool expected)
    {
        var context = new DummyAnalysisContext(TestContext, unchangedFileName);
        var sut = new SonarParametrizedAnalysisContext(new(context, DummyMainDescriptor));
        sut.RegisterNodeAction<SyntaxKind>(CSharpGeneratedCodeRecognizer.Instance, context.DelegateAction);

        context.AssertDelegateInvoked(expected);
    }

    [DataTestMethod]
    [DataRow("snippet1.cs")]
    [DataRow("Other file is unchanged.cs")]
    public void RegisterNodeActionInAllFiles_UnchangedFiles_GeneratedFiles_AlwaysRuns(string unchangedFileName) =>
        new VerifierBuilder<DummyAnalyzerForGenerated>()
            .WithSonarProjectConfigPath(AnalysisScaffolding.CreateSonarProjectConfigWithUnchangedFiles(TestContext, unchangedFileName))
            .AddSnippet("""
                        // <auto-generated/>
                        public class Something { } // Noncompliant
                        """)
            .Verify();

    [DataTestMethod]
    [DataRow(SnippetFileName, false)]
    [DataRow(AnotherFileName, true)]
    public void RegisterTreeAction_UnchangedFiles_SonarAnalysisContext(string unchangedFileName, bool expected)
    {
        var context = new DummyAnalysisContext(TestContext, unchangedFileName);
        var sut = new SonarAnalysisContext(context, DummyMainDescriptor);
        sut.RegisterTreeAction(CSharpGeneratedCodeRecognizer.Instance, context.DelegateAction);

        context.AssertDelegateInvoked(expected);
    }

    [DataTestMethod]
    [DataRow(SnippetFileName, false)]
    [DataRow(AnotherFileName, true)]
    public void RegisterTreeAction_UnchangedFiles_SonarParametrizedAnalysisContext(string unchangedFileName, bool expected)
    {
        var context = new DummyAnalysisContext(TestContext, unchangedFileName);
        var sut = new SonarParametrizedAnalysisContext(new(context, DummyMainDescriptor));
        sut.RegisterTreeAction(CSharpGeneratedCodeRecognizer.Instance, context.DelegateAction);
        sut.ExecutePostponedActions(new(sut, MockCompilationStartAnalysisContext(context)));  // Manual invocation, because SonarParametrizedAnalysisContext stores actions separately

        context.AssertDelegateInvoked(expected);
    }

    [TestMethod]
    public void RegisterTreeAction_Extension_SonarParametrizedAnalysisContext_CS()
    {
        var context = new DummyAnalysisContext(TestContext);
        var self = new SonarParametrizedAnalysisContext(new(context, DummyMainDescriptor));
        CS.RegisterTreeAction(self, context.DelegateAction);
        self.ExecutePostponedActions(new(self, MockCompilationStartAnalysisContext(context)));  // Manual invocation, because SonarParametrizedAnalysisContext stores actions separately

        context.AssertDelegateInvoked(true);
    }

    [TestMethod]
    public void RegisterTreeAction_Extension_SonarParametrizedAnalysisContext_VB()
    {
        var context = new DummyAnalysisContext(TestContext);
        var self = new SonarParametrizedAnalysisContext(new(context, DummyMainDescriptor));
        VB.RegisterTreeAction(self, context.DelegateAction);
        self.ExecutePostponedActions(new(self, MockCompilationStartAnalysisContext(context)));  // Manual invocation, because SonarParametrizedAnalysisContext stores actions separately

        context.AssertDelegateInvoked(true);
    }

    [DataTestMethod]
    [DataRow(SnippetFileName, false)]
    [DataRow(AnotherFileName, true)]
    public void RegisterCodeBlockStartAction_UnchangedFiles_SonarAnalysisContext(string unchangedFileName, bool expected)
    {
        var context = new DummyAnalysisContext(TestContext, unchangedFileName);
        var sut = new SonarAnalysisContext(context, DummyMainDescriptor);
        sut.RegisterCodeBlockStartAction<SyntaxKind>(CSharpGeneratedCodeRecognizer.Instance, context.DelegateAction);

        context.AssertDelegateInvoked(expected);
    }

    [TestMethod]
    public void SonarCompilationStartAnalysisContext_RegisterCompilationEndAction()
    {
        var context = new DummyAnalysisContext(TestContext);
        var startContext = new DummyCompilationStartAnalysisContext(context);
        var sut = new SonarCompilationStartAnalysisContext(new(context, DummyMainDescriptor), startContext);
        sut.RegisterCompilationEndAction(_ => { });

        startContext.AssertExpectedInvocationCounts(expectedCompilationEndCount: 1);
    }

    [TestMethod]
    public void SonarCompilationStartAnalysisContext_RegisterSymbolAction()
    {
        var context = new DummyAnalysisContext(TestContext);
        var startContext = new DummyCompilationStartAnalysisContext(context);
        var sut = new SonarCompilationStartAnalysisContext(new(context, DummyMainDescriptor), startContext);
        sut.RegisterSymbolAction(_ => { });

        startContext.AssertExpectedInvocationCounts(expectedSymbolCount: 1);
    }

    [TestMethod]
    public void SonarCompilationStartAnalysisContext_RegisterNodeAction()
    {
        var context = new DummyAnalysisContext(TestContext);
        var startContext = new DummyCompilationStartAnalysisContext(context);
        var sut = new SonarCompilationStartAnalysisContext(new(context, DummyMainDescriptor), startContext);
        sut.RegisterNodeAction<SyntaxKind>(CSharpGeneratedCodeRecognizer.Instance, _ => { });

        startContext.AssertExpectedInvocationCounts(expectedNodeCount: 0); // RegisterNodeAction doesn't use DummyCompilationStartAnalysisContext to register but a newly created context
    }

    [TestMethod]
    public void SonarCompilationStartAnalysisContext_RegisterCompilationEnd_ReportsIssue()
    {
        var context = new DummyAnalysisContext(TestContext);
        var startContext = new DummyCompilationStartAnalysisContext(context);
        var sut = new SonarCompilationStartAnalysisContext(new(context, DummyMainDescriptor), startContext);
        var diagnostic = Diagnostic.Create(DiagnosticDescriptorFactory.CreateUtility("TEST", "Test report"), context.Tree.GetRoot().GetLocation());
        sut.RegisterCompilationEndAction(x => x.ReportIssue(CSharpGeneratedCodeRecognizer.Instance, diagnostic));

        startContext.RaisedDiagnostic.Should().NotBeNull().And.BeSameAs(diagnostic);
    }

    [TestMethod]
    public void SonarCompilationStartAnalysisContext_RegistSymbol_ReportsIssue()
    {
        var context = new DummyAnalysisContext(TestContext);
        var startContext = new DummyCompilationStartAnalysisContext(context);
        var sut = new SonarCompilationStartAnalysisContext(new(context, DummyMainDescriptor), startContext);
        var diagnostic = Diagnostic.Create(DiagnosticDescriptorFactory.CreateUtility("TEST", "Test report"), context.Tree.GetRoot().GetLocation());
        sut.RegisterSymbolAction(x => x.ReportIssue(CSharpGeneratedCodeRecognizer.Instance, diagnostic));

        startContext.RaisedDiagnostic.Should().NotBeNull().And.BeSameAs(diagnostic);
    }

#if NET

    [DataTestMethod]
    [DataRow("S109", "razor")]
    [DataRow("S103", "razor")]
    [DataRow("S1192", "razor")]
    [DataRow("S104", "razor")]
    [DataRow("S113", "razor")]
    [DataRow("S1451", "razor")]
    [DataRow("S1147", "razor")]
    [DataRow("S109", "cshtml")]
    [DataRow("S103", "cshtml")]
    [DataRow("S1192", "cshtml")]
    [DataRow("S104", "cshtml")]
    [DataRow("S113", "cshtml")]
    [DataRow("S1451", "cshtml")]
    [DataRow("S1147", "cshtml")]
    public void DisabledRules_ForRazor_DoNotRaise(string ruleId, string extension)
    {
        using var scope = new EnvironmentVariableScope(false) { EnableRazorAnalysis = true };
        new VerifierBuilder()
            .AddAnalyzer(() => new DummyAnalyzerWithLocation(ruleId, DiagnosticDescriptorFactory.MainSourceScopeTag))
            .WithSonarProjectConfigPath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
            .AddSnippet("""
                        @code
                        {
                            private int magicNumber = RaiseHere();
                            private static int RaiseHere()
                            {
                                return 42;
                            }
                        }
                        """,
                        $"SomeFile.{extension}")
            .VerifyNoIssueReported();
    }

    [DataTestMethod]
    [DataRow("razor")]
    [DataRow("cshtml")]
    public void TestRules_ForRazor_DoNotRaise(string extension)
    {
        using var scope = new EnvironmentVariableScope(false) { EnableRazorAnalysis = true };
        new VerifierBuilder()
            .AddAnalyzer(() => new DummyAnalyzerWithLocation("DummyId", DiagnosticDescriptorFactory.TestSourceScopeTag))
            .WithSonarProjectConfigPath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Test))
            .AddSnippet("""
                        @code
                        {
                            private int magicNumber = RaiseHere();
                            private static int RaiseHere()
                            {
                                return 42;
                            }
                        }
                        """,
                        $"SomeFile.{extension}")
            .VerifyNoIssueReported();
    }

    [DataTestMethod]
    [DataRow("razor")]
    [DataRow("cshtml")]
    public void AllScopedRules_ForRazor_Raise(string extension)
    {
        using var scope = new EnvironmentVariableScope(false) { EnableRazorAnalysis = true };
        new VerifierBuilder()
            .AddAnalyzer(() => new DummyAnalyzerWithLocation("DummyId", DiagnosticDescriptorFactory.TestSourceScopeTag, DiagnosticDescriptorFactory.MainSourceScopeTag))
            .WithSonarProjectConfigPath(AnalysisScaffolding.CreateSonarProjectConfig(TestContext, ProjectType.Product))
            .AddSnippet("""
                        @code
                        {
                            private int magicNumber = RaiseHere(); // Noncompliant
                            private static int RaiseHere()
                            {
                                return 42;
                            }
                        }
                        """,
                        $"SomeFile.{extension}")
            .Verify();
    }

#endif

    private static CompilationStartAnalysisContext MockCompilationStartAnalysisContext(DummyAnalysisContext context)
    {
        var mock = new Mock<CompilationStartAnalysisContext>(context.Model.Compilation, context.Options, CancellationToken.None);
        mock.Setup(x => x.RegisterSyntaxNodeAction(It.IsAny<Action<SyntaxNodeAnalysisContext>>(), It.IsAny<ImmutableArray<SyntaxKind>>()))
            .Callback<Action<SyntaxNodeAnalysisContext>, ImmutableArray<SyntaxKind>>((action, _) => action(context.CreateSyntaxNodeAnalysisContext())); // Invoke to call RegisterSyntaxTreeAction
        mock.Setup(x => x.RegisterSyntaxTreeAction(It.IsAny<Action<SyntaxTreeAnalysisContext>>()))
            .Callback<Action<SyntaxTreeAnalysisContext>>(x => x(new SyntaxTreeAnalysisContext(context.Tree, context.Options, _ => { }, _ => true, default)));
        return mock.Object;
    }

    private sealed class DummyAnalysisContext : RoslynAnalysisContext
    {
        public readonly AnalyzerOptions Options;
        public readonly SemanticModel Model;
        public readonly SyntaxTree Tree;
        private bool delegateWasInvoked;

        public DummyAnalysisContext(TestContext testContext, params string[] unchangedFiles)
        {
            Options = AnalysisScaffolding.CreateOptions(AnalysisScaffolding.CreateSonarProjectConfigWithUnchangedFiles(testContext, unchangedFiles));
            (Tree, Model) = TestHelper.CompileCS("public class Sample { }");
        }

        public void DelegateAction<T>(T arg) =>
            delegateWasInvoked = true;

        public void AssertDelegateInvoked(bool expected, string because = "") =>
            delegateWasInvoked.Should().Be(expected, because);

        public SyntaxNodeAnalysisContext CreateSyntaxNodeAnalysisContext() =>
            new(Tree.GetRoot(), Model, Options, _ => { }, _ => true, default);

        public override void RegisterCodeBlockAction(Action<CodeBlockAnalysisContext> action) =>
            throw new NotImplementedException();

        public override void RegisterCodeBlockStartAction<TLanguageKindEnum>(Action<CodeBlockStartAnalysisContext<TLanguageKindEnum>> action) =>
            action(new DummyCodeBlockStartAnalysisContext<TLanguageKindEnum>(this));

        public override void RegisterCompilationAction(Action<CompilationAnalysisContext> action) =>
            throw new NotImplementedException();

        public override void RegisterCompilationStartAction(Action<CompilationStartAnalysisContext> action) =>
            action(MockCompilationStartAnalysisContext(this));  // Directly invoke to let the inner registrations be added into this.actions

        public override void RegisterSemanticModelAction(Action<SemanticModelAnalysisContext> action) =>
            throw new NotImplementedException();

        public override void RegisterSymbolAction(Action<SymbolAnalysisContext> action, ImmutableArray<SymbolKind> symbolKinds) =>
            throw new NotImplementedException();

        public override void RegisterSyntaxNodeAction<TLanguageKindEnum>(Action<SyntaxNodeAnalysisContext> action, ImmutableArray<TLanguageKindEnum> syntaxKinds) =>
            action(CreateSyntaxNodeAnalysisContext());

        public override void RegisterSyntaxTreeAction(Action<SyntaxTreeAnalysisContext> action) =>
            throw new NotImplementedException();
    }

    private class DummyCodeBlockStartAnalysisContext<TSyntaxKind> : CodeBlockStartAnalysisContext<TSyntaxKind> where TSyntaxKind : struct
    {
        public DummyCodeBlockStartAnalysisContext(DummyAnalysisContext baseContext) : base(baseContext.Tree.GetRoot(), null, baseContext.Model, baseContext.Options, default) { }

        public override void RegisterCodeBlockEndAction(Action<CodeBlockAnalysisContext> action) =>
            throw new NotImplementedException();

        public override void RegisterSyntaxNodeAction(Action<SyntaxNodeAnalysisContext> action, ImmutableArray<TSyntaxKind> syntaxKinds) =>
            throw new NotImplementedException();
    }

    private class DummyCompilationStartAnalysisContext : CompilationStartAnalysisContext
    {
        private readonly DummyAnalysisContext context;
        private int compilationEndCount;
        private int semanticModelCount;
        private int symbolCount;
        private int nodeCount;

        public Diagnostic RaisedDiagnostic { get; private set; }

        public DummyCompilationStartAnalysisContext(DummyAnalysisContext context) : base(context.Model.Compilation, context.Options, default) =>
            this.context = context;

        public void AssertExpectedInvocationCounts(int expectedCompilationEndCount = 0, int expectedSemanticModelCount = 0, int expectedSymbolCount = 0, int expectedNodeCount = 0)
        {
            compilationEndCount.Should().Be(expectedCompilationEndCount);
            semanticModelCount.Should().Be(expectedSemanticModelCount);
            symbolCount.Should().Be(expectedSymbolCount);
            nodeCount.Should().Be(expectedNodeCount);
        }

        public override void RegisterCodeBlockAction(Action<CodeBlockAnalysisContext> action) =>
            throw new NotImplementedException();

        public override void RegisterCodeBlockStartAction<TLanguageKindEnum>(Action<CodeBlockStartAnalysisContext<TLanguageKindEnum>> action) =>
            throw new NotImplementedException();

        public override void RegisterCompilationEndAction(Action<CompilationAnalysisContext> action)
        {
            compilationEndCount++;
            action(new CompilationAnalysisContext(context.Model.Compilation, context.Options, reportDiagnostic: x => RaisedDiagnostic = x, isSupportedDiagnostic: _ => true, CancellationToken.None));
        }

        public override void RegisterSemanticModelAction(Action<SemanticModelAnalysisContext> action)
        {
            semanticModelCount++;
            action(new SemanticModelAnalysisContext(context.Model, context.Options, reportDiagnostic: x => RaisedDiagnostic = x, isSupportedDiagnostic: _ => true, CancellationToken.None));
        }

        public override void RegisterSymbolAction(Action<SymbolAnalysisContext> action, ImmutableArray<SymbolKind> symbolKinds)
        {
            symbolCount++;
            action(new SymbolAnalysisContext(Mock.Of<ISymbol>(), context.Model.Compilation, context.Options,
                reportDiagnostic: x => RaisedDiagnostic = x, isSupportedDiagnostic: _ => true, CancellationToken.None));
        }

        public override void RegisterSyntaxNodeAction<TLanguageKindEnum>(Action<SyntaxNodeAnalysisContext> action, ImmutableArray<TLanguageKindEnum> syntaxKinds) =>
            nodeCount++;

        public override void RegisterSyntaxTreeAction(Action<SyntaxTreeAnalysisContext> action) =>
            throw new NotImplementedException();
    }

    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    private class DummyAnalyzerForGenerated : SonarDiagnosticAnalyzer
    {
        private readonly DiagnosticDescriptor rule = AnalysisScaffolding.CreateDescriptorMain();

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(rule);

        protected override void Initialize(SonarAnalysisContext context) =>
            context.RegisterNodeActionInAllFiles(c => c.ReportIssue(Diagnostic.Create(rule, c.Node.GetLocation())), SyntaxKind.ClassDeclaration);
    }
}
