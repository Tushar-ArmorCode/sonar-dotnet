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

using System.Collections.Concurrent;
using System.IO;
using System.Runtime.CompilerServices;
using static SonarAnalyzer.Helpers.DiagnosticDescriptorFactory;

namespace SonarAnalyzer.AnalysisContext;

public class SonarAnalysisContextBase
{
    protected static readonly ConditionalWeakTable<Compilation, ConcurrentDictionary<string, bool>> FileInclusionCache = new();
    protected static readonly ConditionalWeakTable<Compilation, ImmutableHashSet<string>> UnchangedFilesCache = new();
    protected static readonly SourceTextValueProvider<ProjectConfigReader> ProjectConfigProvider = new(x => new ProjectConfigReader(x));
    protected static readonly SourceTextValueProvider<SonarLintXmlReader> SonarLintXmlProviderCS = new(x => new SonarLintXmlReader(x, LanguageNames.CSharp));
    protected static readonly SourceTextValueProvider<SonarLintXmlReader> SonarLintXmlProviderVB = new(x => new SonarLintXmlReader(x, LanguageNames.VisualBasic));

    protected SonarAnalysisContextBase() { }

    protected static SourceTextValueProvider<SonarLintXmlReader> SonarLintXmlReader(string language) =>
        language == LanguageNames.CSharp ? SonarLintXmlProviderCS : SonarLintXmlProviderVB;
}

public abstract class SonarAnalysisContextBase<TContext> : SonarAnalysisContextBase
{
    public abstract SyntaxTree Tree { get; }
    public abstract Compilation Compilation { get; }
    public abstract AnalyzerOptions Options { get; }
    public abstract CancellationToken Cancel { get; }

    public SonarAnalysisContext AnalysisContext { get; }
    public TContext Context { get; }

    protected SonarAnalysisContextBase(SonarAnalysisContext analysisContext, TContext context)
    {
        AnalysisContext = analysisContext ?? throw new ArgumentNullException(nameof(analysisContext));
        Context = context;
    }

    /// <param name="tree">Tree to decide on. Can be null for Symbol-based and Compilation-based scenarios. And we want to analyze those too.</param>
    /// <param name="generatedCodeRecognizer">When set, generated trees are analyzed only when language-specific 'analyzeGeneratedCode' configuration property is also set.</param>
    public bool ShouldAnalyzeTree(SyntaxTree tree, GeneratedCodeRecognizer generatedCodeRecognizer) =>
        (generatedCodeRecognizer is null || SonarLintFile().AnalyzeGeneratedCode || !tree.IsGenerated(generatedCodeRecognizer, Compilation))
        && (tree is null || (!IsUnchanged(tree) && ShouldAnalyzeFile(tree.FilePath)));

    /// <summary>
    /// Reads configuration from SonarProjectConfig.xml file and caches the result for scope of this analysis.
    /// </summary>
    public ProjectConfigReader ProjectConfiguration()
    {
        if (Options.SonarProjectConfig() is { } sonarProjectConfig)
        {
            return sonarProjectConfig.GetText() is { } sourceText
                // TryGetValue catches all exceptions from SourceTextValueProvider and returns false when exception is thrown
                && AnalysisContext.TryGetValue(sourceText, ProjectConfigProvider, out var cachedProjectConfigReader)
                ? cachedProjectConfigReader
                : throw new InvalidOperationException($"File '{Path.GetFileName(sonarProjectConfig.Path)}' has been added as an AdditionalFile but could not be read and parsed.");
        }
        else
        {
            return ProjectConfigReader.Empty;
        }
    }

    /// <summary>
    /// Reads the properties from the SonarLint.xml file and caches the result for the scope of this analysis.
    /// </summary>
    public SonarLintXmlReader SonarLintFile()
    {
        if (Options.SonarLintXml() is { } sonarLintXml)
        {
            return sonarLintXml.GetText() is { } sourceText
                && AnalysisContext.TryGetValue(sourceText, SonarLintXmlReader(Compilation.Language), out var sonarLintXmlReader)
                ? sonarLintXmlReader
                : throw new InvalidOperationException($"File '{Path.GetFileName(sonarLintXml.Path)}' has been added as an AdditionalFile but could not be read and parsed.");
        }
        else
        {
            return Helpers.SonarLintXmlReader.Empty;
        }
    }

    public bool IsTestProject()
    {
        var projectType = ProjectConfiguration().ProjectType;
        return projectType == ProjectType.Unknown
            ? Compilation.IsTest()              // SonarLint, NuGet or Scanner <= 5.0
            : projectType == ProjectType.Test;  // Scanner >= 5.1 does authoritative decision that we follow
    }

    public bool IsUnchanged(SyntaxTree tree) =>
        UnchangedFilesCache.GetValue(Compilation, _ => CreateUnchangedFilesHashSet()).Contains(tree.FilePath);

    public bool HasMatchingScope(IEnumerable<DiagnosticDescriptor> descriptors) =>
        descriptors.Any(HasMatchingScope);

    public bool HasMatchingScope(DiagnosticDescriptor descriptor)
    {
        // MMF-2297: Test Code as 1st Class Citizen is not ready on server side yet.
        // ScannerRun: Only utility rules and rules with TEST-ONLY scope are executed for test projects for now.
        // SonarLint & Standalone NuGet: Respect the scope as before.
        return IsTestProject()
            ? ContainsTag(TestSourceScopeTag) && !(ProjectConfiguration().IsScannerRun && ContainsTag(MainSourceScopeTag) && !ContainsTag(UtilityTag))
            : ContainsTag(MainSourceScopeTag);

        bool ContainsTag(string tag) =>
            descriptor.CustomTags.Contains(tag);
    }

    public bool ShouldAnalyzeFile(string filePath) =>
        ProjectConfiguration().ProjectType == ProjectType.Unknown // SonarLint, NuGet or Scanner <= 5.0
        && FileInclusionCache.GetValue(Compilation, _ => new()) is var cache
        && cache.GetOrAdd(filePath, _ => IsFileIncluded(filePath));

    private ImmutableHashSet<string> CreateUnchangedFilesHashSet() =>
        ImmutableHashSet.Create(StringComparer.OrdinalIgnoreCase, ProjectConfiguration().AnalysisConfig?.UnchangedFiles() ?? Array.Empty<string>());

    private bool ShouldAnalyzeGenerated() =>
        Options.SonarLintXml() is { } sonarLintXml
        && AnalysisContext.TryGetValue(sonarLintXml.GetText(), ShouldAnalyzeGeneratedProvider(Compilation.Language), out var shouldAnalyzeGenerated)
        && shouldAnalyzeGenerated;

    private bool IsFileIncluded(string filePath) =>
        IsTestProject()
        ? IsFileIncluded(SonarLintFile().TestInclusions, SonarLintFile().TestExclusions, SonarLintFile().GlobalTestExclusions, filePath)
        : IsFileIncluded(SonarLintFile().Inclusions, SonarLintFile().Exclusions, SonarLintFile().GlobalExclusions, filePath);

    private static bool IsFileIncluded(string[] inclusions, string[] exclusions, string[] globalExclusions, string filePath) =>
        IsIncluded(inclusions, filePath)
        && !IsExcluded(exclusions, filePath)
        && !IsExcluded(globalExclusions, filePath);

    private static bool IsIncluded(string[] inclusions, string filePath) =>
        inclusions is { Length: 0 } || inclusions.Any(x => WildcardPatternMatcher.IsMatch(x, filePath));

    private static bool IsExcluded(string[] exclusions, string filePath) =>
        exclusions.Any(x => WildcardPatternMatcher.IsMatch(x, filePath));
}
