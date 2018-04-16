﻿/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2018 SonarSource SA
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

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [Rule(DiagnosticId)]
    public sealed class LdapConnectionsShouldBeSecure : ObjectsShouldBeInitializedCorrectlyBase<int>
    {
        internal const string DiagnosticId = "S4433";
        private const string MessageFormat = "Set the 'AuthenticationType' property of this DirectoryEntry to 'AuthenticationTypes.Secure'.";
        private const int AuthenticationTypes_Secure = 1;

        private static readonly DiagnosticDescriptor rule =
            DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, RspecStrings.ResourceManager);

        protected override DiagnosticDescriptor Rule => rule;

        internal override KnownType TrackedType => KnownType.System_DirectoryServices_DirectoryEntry;

        protected override string TrackedPropertyName => "AuthenticationType";

        protected override int ExpectedPropertyValue => AuthenticationTypes_Secure;

        protected override int CtorArgumentsCount => 4;

        protected override int CtorArgumentIndex => 3;

        protected override bool ExpectedValueIsDefault => true;

        protected override bool IsExpectedValue(object constantValue) =>
            constantValue is int integerValue &&
            (integerValue & AuthenticationTypes_Secure) > 0; // The expected value is a bit from a Flags enum
    }
}
