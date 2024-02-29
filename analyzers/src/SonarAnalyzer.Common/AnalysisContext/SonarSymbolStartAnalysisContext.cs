﻿/*
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

using SonarAnalyzer.ShimLayer.AnalysisContext;

namespace SonarAnalyzer.AnalysisContext;

public sealed class SonarSymbolStartAnalysisContext : SonarAnalysisContextBase<SymbolStartAnalysisContext>
{
    public override Compilation Compilation => Context.Compilation;
    public override AnalyzerOptions Options => Context.Options;
    public override CancellationToken Cancel => Context.CancellationToken;
    public ISymbol Symbol => Context.Symbol;

    internal SonarSymbolStartAnalysisContext(SonarAnalysisContext analysisContext, SymbolStartAnalysisContext context) : base(analysisContext, context) { }

    public void RegisterCodeBlockAction(Action<SonarCodeBlockReportingContext> action)
        => Context.RegisterCodeBlockAction(x => action(new(AnalysisContext, x)));

    public void RegisterCodeBlockStartAction<TLanguageKindEnum>(Action<SonarCodeBlockStartAnalysisContext<TLanguageKindEnum>> action) where TLanguageKindEnum : struct
        => Context.RegisterCodeBlockStartAction<TLanguageKindEnum>(x => action(new(AnalysisContext, x)));

    public void RegisterOperationAction(Action<OperationAnalysisContext> action, ImmutableArray<OperationKind> operationKinds)
        => Context.RegisterOperationAction(action, operationKinds);

    public void RegisterOperationBlockAction(Action<OperationBlockAnalysisContext> action)
        => Context.RegisterOperationBlockAction(action);

    public void RegisterOperationBlockStartAction(Action<OperationBlockStartAnalysisContext> action)
        => Context.RegisterOperationBlockStartAction(action);

    public void RegisterSymbolEndAction(Action<SymbolAnalysisContext> action)
        => Context.RegisterSymbolEndAction(action);

    public void RegisterSyntaxNodeAction<TLanguageKindEnum>(Action<SonarSyntaxNodeReportingContext> action, params TLanguageKindEnum[] syntaxKinds) where TLanguageKindEnum : struct
        => Context.RegisterSyntaxNodeAction(x => action(new(AnalysisContext, x)), syntaxKinds);
}
