/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2024 SonarSource SA
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

#if NET

using System.IO;
using SonarAnalyzer.AnalysisContext;
using static SonarAnalyzer.TestFramework.MetadataReferences.MetadataReferenceFactory;

namespace SonarAnalyzer.TestFramework.MetadataReferences;

public static class SonarAnalyzerMetadataReference
{
    public static MetadataReference SonarAnalyzerCommon { get; } = Create(typeof(SonarAnalysisContext));
    public static MetadataReference SonarAnalyzerCSharp { get; } = CreateFromFile(Path.Combine(Path.GetDirectoryName(typeof(SonarAnalysisContext).Assembly.Location), "SonarAnalyzer.CSharp.dll"));
    public static MetadataReference MicrosoftCodeAnalysis { get; } = Create(typeof(Microsoft.CodeAnalysis.SymbolKind));
    public static MetadataReference MicrosoftCodeAnalysisCsharp { get; } = Create(typeof(Microsoft.CodeAnalysis.CSharp.SyntaxKind));
}
#endif
