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
using System.Text;
using System.Text.RegularExpressions;

namespace SonarAnalyzer.Helpers;

internal static class WildcardPatternMatcher
{
    public static bool IsMatch(string pattern, string input) =>
        !(string.IsNullOrWhiteSpace(pattern) || string.IsNullOrWhiteSpace(input))
        && WildcardPattern.Create(pattern).Match(input);

    /// <summary>
    /// Copied from https://github.com/SonarSource/sonar-plugin-api/blob/a9bd7ff48f0f77811ed909070030678c443c975a/sonar-plugin-api/src/main/java/org/sonar/api/utils/WildcardPattern.java.
    /// </summary>
    private sealed class WildcardPattern
    {
        private const string SpecialChars = "()[]^$.{}+|";
        private static readonly ConcurrentDictionary<string, WildcardPattern> Cache = new();
        private readonly Regex pattern;

        private WildcardPattern(string pattern) =>
            this.pattern = new Regex(ToRegex(pattern), RegexOptions.None, RegexConstants.DefaultTimeout);

        public bool Match(string value)
        {
            try
            {
                return pattern.IsMatch(value.Trim('/'));
            }
            catch (RegexMatchTimeoutException)
            {
                return false;
            }
        }

        public static WildcardPattern Create(string pattern) =>
            Cache.GetOrAdd(pattern + Path.DirectorySeparatorChar, _ => new WildcardPattern(pattern));

        private static string ToRegex(string wildcardPattern)
        {
            var escapedDirectorySeparator = Regex.Escape(Path.DirectorySeparatorChar.ToString());
            var sb = new StringBuilder(wildcardPattern.Length);

            sb.Append('^');

            var i = wildcardPattern.StartsWith("/") || wildcardPattern.StartsWith("\\") ? 1 : 0;
            while (i < wildcardPattern.Length)
            {
                var ch = wildcardPattern[i];

                if (SpecialChars.Contains(ch))
                {
                    // Escape regex-specific characters
                    sb.Append(Regex.Escape(ch.ToString()));
                }
                else if (ch == '*')
                {
                    if (i + 1 < wildcardPattern.Length && wildcardPattern[i + 1] == '*')
                    {
                        // Double asterisk
                        // Zero or more directories
                        if (i + 2 < wildcardPattern.Length && IsSlash(wildcardPattern[i + 2]))
                        {
                            sb.Append($"(.*{escapedDirectorySeparator}|)");
                            i += 2;
                        }
                        else
                        {
                            sb.Append(".*");
                            i += 1;
                        }
                    }
                    else
                    {
                        // Single asterisk
                        // Zero or more characters excluding directory separator
                        sb.Append($"[^{escapedDirectorySeparator}]*?");
                    }
                }
                else if (ch == '?')
                {
                    // Any single character excluding directory separator
                    sb.Append($"[^{escapedDirectorySeparator}]");
                }
                else if (IsSlash(ch))
                {
                    // Directory separator
                    sb.Append(escapedDirectorySeparator);
                }
                else
                {
                    // Single character
                    sb.Append(ch);
                }
                i++;
            }
            return sb.Append('$').ToString();
        }

        private static bool IsSlash(char ch) =>
            ch == '/' || ch == '\\';
    }
}
