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

using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;
using StyleCop.Analyzers.Lightup;

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [Rule(DiagnosticId)]
    public sealed class TypeNamesShouldNotMatchNamespaces : SonarDiagnosticAnalyzer
    {
        private const string DiagnosticId = "S4041";
        private const string MessageFormat = "Change the name of type '{0}' to be different from an existing framework namespace.";

        private static readonly DiagnosticDescriptor Rule = DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, RspecStrings.ResourceManager);
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(Rule);

        // Based on https://msdn.microsoft.com/en-us/library/gg145045%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396
        private static readonly ISet<string> FrameworkNamespaces =
            new HashSet<string>
            {
                "accessibility", "activities", "addin", "build", "codedom", "collections",
                "componentmodel", "configuration", "csharp", "custommarshalers", "data",
                "dataflow", "deployment", "device", "diagnostics", "directoryservices",
                "drawing", "dynamic", "enterpriseservices", "globalization", "identitymodel",
                "interopservices", "io", "jscript", "linq", "location", "management", "media",
                "messaging", "microsoft", "net", "numerics", "printing", "reflection", "resources",
                "runtime", "security", "server", "servicemodel", "serviceprocess", "speech",
                "sqlserver", "system", "tasks", "text", "threading", "timers", "transactions",
                "uiautomationclientsideproviders", "visualbasic", "visualc", "web", "win32",
                "windows", "workflow", "xaml", "xamlgeneratednamespace", "xml"
            };

        protected override void Initialize(SonarAnalysisContext context) =>
            context.RegisterSyntaxNodeActionInNonGenerated(c =>
            {
                if (c.ContainingSymbol.Kind == SymbolKind.NamedType
                    && IsDeclaredPublic(c.Node, c.SemanticModel)
                    && CSharpFacade.Instance.Syntax.NodeIdentifier(c.Node) is { } identifier
                    && FrameworkNamespaces.Contains(identifier.ValueText.ToLowerInvariant()))
                {
                    c.ReportDiagnosticWhenActive(Diagnostic.Create(Rule, identifier.GetLocation(), identifier.ValueText));
                }
            },
            SyntaxKind.ClassDeclaration,
            SyntaxKind.StructDeclaration,
            SyntaxKind.InterfaceDeclaration,
            SyntaxKind.EnumDeclaration,
            SyntaxKind.DelegateDeclaration,
            SyntaxKindEx.RecordDeclaration);

        private static bool IsDeclaredPublic(SyntaxNode declaration, SemanticModel semanticModel) =>
            semanticModel.GetDeclaredSymbol(declaration)?.DeclaredAccessibility == Accessibility.Public;
    }
}
