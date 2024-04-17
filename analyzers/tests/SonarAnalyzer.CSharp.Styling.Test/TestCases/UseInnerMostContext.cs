using SonarAnalyzer;
using SonarAnalyzer.Common;
using SonarAnalyzer.AnalysisContext;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using SonarAnalyzer.Helpers;
using System.Collections.Immutable;

public sealed class Rule1 : SonarDiagnosticAnalyzer
{
    private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor("TC0001",
        "Some title",
        "Some message",
        "Some category",
        DiagnosticSeverity.Warning,
        true);

    // Error@+1 [CS0012]
    public sealed override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule); // Error [CS0012]

    protected override void Initialize(SonarAnalysisContext context) =>
        context.RegisterCompilationStartAction(c =>
        {
            c.RegisterSymbolStartAction(cc =>
            {
                c.RegisterNodeAction(ccc => // Noncompliant {{Use the inner-most 'cc' context}}
//              ^
                {
                    ccc.ReportIssue(Rule, ccc.Node);
                }, SyntaxKind.SimpleMemberAccessExpression);
            }, SymbolKind.Field);
        });
}
