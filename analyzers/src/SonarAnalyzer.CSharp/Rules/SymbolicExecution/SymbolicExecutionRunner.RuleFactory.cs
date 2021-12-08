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
using SonarAnalyzer.SymbolicExecution.Roslyn;

namespace SonarAnalyzer.Rules.SymbolicExecution
{
    public partial class SymbolicExecutionRunner
    {
        private static RuleFactory CreateFactory<TRuleCheck>() where TRuleCheck : SymbolicRuleCheck, new() =>
            new RuleFactory<TRuleCheck>();

        private class RuleFactory
        {
            public readonly Type Type;
            private readonly Func<SymbolicRuleCheck> createInstance;

            protected RuleFactory(Type type, Func<SymbolicRuleCheck> createInstance)
            {
                Type = type;
                this.createInstance = createInstance;
            }

            public SymbolicRuleCheck CreateInstance() =>
                createInstance();
        }

        private sealed class RuleFactory<TCheck> : RuleFactory
            where TCheck : SymbolicRuleCheck, new()
        {
            public RuleFactory() : base(typeof(TCheck), () => new TCheck()) { }
        }
    }
}
