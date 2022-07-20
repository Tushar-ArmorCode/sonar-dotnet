/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2022 SonarSource SA
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
using SonarAnalyzer.Rules.CSharp;

namespace SonarAnalyzer.UnitTest.Rules
{
    [TestClass]
    public class RedundantCastTest
    {
        private readonly VerifierBuilder builder = new VerifierBuilder<RedundantCast>();

        [Ignore][TestMethod]
        public void RedundantCast() =>
            builder.AddPaths("RedundantCast.cs").Verify();

        [Ignore][TestMethod]
        public void RedundantCast_CSharp8() =>
            builder.AddPaths("RedundantCast.CSharp8.cs").WithOptions(ParseOptionsHelper.FromCSharp8).Verify();

#if NET
        [Ignore][TestMethod]
        public void RedundantCast_CSharp9() =>
            builder.AddPaths("RedundantCast.CSharp9.cs").WithOptions(ParseOptionsHelper.FromCSharp9).Verify();
#endif

        [Ignore][TestMethod]
        public void RedundantCast_CodeFix() =>
            builder.AddPaths("RedundantCast.cs").WithCodeFix<RedundantCastCodeFix>().WithCodeFixedPaths("RedundantCast.Fixed.cs").VerifyCodeFix();

        [Ignore][TestMethod]
        public void RedundantCast_DefaultLiteral() =>
            builder.AddSnippet(@"
using System;
public static class MyClass
{
    public static void RunAction(Action action)
    {
        bool myBool = (bool)default; // FN - the cast is unneeded
        RunFunc(() => { action(); return default; }, (bool)default); // should not raise because of the generic the cast is mandatory
        RunFunc<bool>(() => { action(); return default; }, (bool)default); // FN - the cast is unneeded
    }

     public static T RunFunc<T>(Func<T> func, T returnValue = default) => returnValue;
}").WithLanguageVersion(LanguageVersion.CSharp7_1).Verify();
    }
}
