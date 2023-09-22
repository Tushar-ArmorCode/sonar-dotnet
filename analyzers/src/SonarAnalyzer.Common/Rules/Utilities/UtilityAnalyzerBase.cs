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
using Google.Protobuf;
using SonarAnalyzer.Protobuf;

namespace SonarAnalyzer.Rules
{
    public abstract class UtilityAnalyzerBase : SonarDiagnosticAnalyzer
    {
        protected static readonly ISet<string> FileExtensionWhitelist = new HashSet<string> { ".cs", ".csx", ".vb" };
        private readonly DiagnosticDescriptor rule;

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(rule);
        protected bool IsAnalyzerEnabled { get; set; }
        protected bool IgnoreHeaderComments { get; set; }
        protected virtual bool AnalyzeGeneratedCode { get; set; }
        protected virtual bool AnalyzeTestProjects => true;
        protected string OutPath { get; set; }
        protected bool IsTestProject { get; set; }
        protected override bool EnableConcurrentExecution => false;

        protected UtilityAnalyzerBase(string diagnosticId, string title) =>
            rule = DiagnosticDescriptorFactory.CreateUtility(diagnosticId, title);

        internal static TextRange GetTextRange(FileLinePositionSpan lineSpan) =>
            new()
            {
                StartLine = lineSpan.StartLinePosition.GetLineNumberToReport(),
                EndLine = lineSpan.EndLinePosition.GetLineNumberToReport(),
                StartOffset = lineSpan.StartLinePosition.Character,
                EndOffset = lineSpan.EndLinePosition.Character
            };

        protected void ReadParameters(SonarCompilationReportingContext context)
        {
            var outPath = context.ProjectConfiguration().OutPath;
            // For backward compatibility with S4MSB <= 5.0
            if (outPath == null && context.Options.ProjectOutFolderPath() is { } projectOutFolderAdditionalFile)
            {
                outPath = projectOutFolderAdditionalFile.GetText().ToString().TrimEnd();
            }
            if (context.Options.SonarLintXml() != null && !string.IsNullOrEmpty(outPath))
            {
                var sonarLintXml = context.SonarLintXml();
                IgnoreHeaderComments = sonarLintXml.IgnoreHeaderComments(context.Compilation.Language);
                AnalyzeGeneratedCode = sonarLintXml.AnalyzeGeneratedCode(context.Compilation.Language);
                OutPath = Path.Combine(outPath, context.Compilation.Language == LanguageNames.CSharp ? "output-cs" : "output-vbnet");
                IsAnalyzerEnabled = true;
                IsTestProject = context.IsTestProject();
            }
        }
    }

    public abstract class UtilityAnalyzerBase<TSyntaxKind, TMessage> : UtilityAnalyzerBase
        where TSyntaxKind : struct
        where TMessage : class, IMessage, new()
    {
        protected abstract ILanguageFacade<TSyntaxKind> Language { get; }
        protected abstract string FileName { get; }
        protected abstract TMessage CreateMessage(SyntaxTree tree, SemanticModel model, ImmutableSortedSet<LineDirectiveEntry> lineDirectiveMap);

        protected virtual bool AnalyzeUnchangedFiles => false;

        protected virtual IEnumerable<TMessage> CreateAnalysisMessages(SonarCompilationReportingContext c) => Enumerable.Empty<TMessage>();

        protected virtual ImmutableSortedSet<LineDirectiveEntry> CalculateLineDirectiveMap(SyntaxTree syntaxTree) => ImmutableSortedSet<LineDirectiveEntry>.Empty;

        protected UtilityAnalyzerBase(string diagnosticId, string title) : base(diagnosticId, title) { }

        protected sealed override void Initialize(SonarAnalysisContext context) =>
            context.RegisterCompilationAction(context =>
            {
                ReadParameters(context);
                if (!IsAnalyzerEnabled)
                {
                    return;
                }

                var treeMessages = context.Compilation.SyntaxTrees
                    .Where(x => ShouldGenerateMetrics(context, x))
                    .Select(x =>
                        {
                            var lineDirectiveMap = ImmutableSortedSet<LineDirectiveEntry>.Empty;
                            if (GeneratedCodeRecognizer.IsRazorGeneratedFile(x))
                            {
                                lineDirectiveMap = CalculateLineDirectiveMap(x);
                            }
                            return CreateMessage(x, context.Compilation.GetSemanticModel(x), lineDirectiveMap);
                        });

                var allMessages = CreateAnalysisMessages(context)
                    .Concat(treeMessages)
                    .WhereNotNull()
                    .ToArray();

                Directory.CreateDirectory(OutPath);
                using var stream = File.Create(Path.Combine(OutPath, FileName));
                foreach (var message in allMessages)
                {
                    message.WriteDelimitedTo(stream);
                }
            });

        protected virtual bool ShouldGenerateMetrics(SyntaxTree tree, Compilation compilation) =>
            // The results of Metrics and CopyPasteToken analyzers are not needed for Test projects yet the plugin side expects the protobuf files, so we create empty ones.
            (AnalyzeTestProjects || !IsTestProject)
            && FileExtensionWhitelist.Contains(Path.GetExtension(tree.FilePath))
            && ShouldGenerateMetricsByType(tree, compilation);

        protected static string GetFilePath(SyntaxTree tree) =>
            // If the syntax tree is constructed for a razor generated file, we need to provide the original file path.
            GeneratedCodeRecognizer.IsRazorGeneratedFile(tree) && tree.GetRoot() is var root && root.ContainsDirectives
                ? root.GetMappedFilePathFromRoot()
                : tree.FilePath;

        private bool ShouldGenerateMetrics(SonarCompilationReportingContext context, SyntaxTree tree) =>
            (AnalyzeUnchangedFiles || !context.IsUnchanged(tree))
            && ShouldGenerateMetrics(tree, context.Compilation);

        private bool ShouldGenerateMetricsByType(SyntaxTree tree, Compilation compilation) =>
            AnalyzeGeneratedCode
                ? !GeneratedCodeRecognizer.IsCshtml(tree) // We cannot upload metrics for .cshtml files. The file is owned by the html plugin.
                : !tree.IsGenerated(Language.GeneratedCodeRecognizer, compilation) || GeneratedCodeRecognizer.IsRazor(tree);
    }
}
