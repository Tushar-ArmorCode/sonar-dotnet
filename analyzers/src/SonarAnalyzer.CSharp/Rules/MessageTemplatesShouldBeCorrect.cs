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

using System.Text.RegularExpressions;

namespace SonarAnalyzer.Rules.CSharp;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class MessageTemplatesShouldBeCorrect : SonarDiagnosticAnalyzer
{
    private const string DiagnosticId = "S6674";
    private const string MessageFormat = "Log message template {0}.";

    private static readonly DiagnosticDescriptor Rule = DescriptorFactory.Create(DiagnosticId, MessageFormat);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

    protected override void Initialize(SonarAnalysisContext context) =>
        context.RegisterNodeAction(c =>
        {
            var invocation = (InvocationExpressionSyntax)c.Node;
            if (MessageTemplateAnalyzer.TemplateArgument(invocation, c.SemanticModel) is { } argument
                && argument.Expression.IsKind(SyntaxKind.StringLiteralExpression)
                && TemplateValidator.ContainsErrors(argument.Expression.ToString(), out var errors))
            {
                var templateStart = argument.Expression.GetLocation().SourceSpan.Start;
                foreach (var error in errors)
                {
                    var location = Location.Create(c.Tree, new(templateStart + error.Start, error.Length));
                    c.ReportIssue(Diagnostic.Create(Rule, location, error.Message));
                }
            }
        },
        SyntaxKind.InvocationExpression);

    private static class TemplateValidator
    {
        private const string TextPattern = @"([^\{]|\{\{|\}\})+";
        private const string HolePattern = @"{(?<Placeholder>[^\}]*)}";
        private const string TemplatePattern = $"^({TextPattern}|{HolePattern})*$";
        // This is similar to the regex used for MessageTemplatesAnalyzer, but it is far more permissive.
        // The goal is to manually parse the placeholders, so that we can report more specific issues than just "malformed template".
        private static readonly Regex TemplateRegex = new(TemplatePattern, RegexOptions.Compiled, TimeSpan.FromMilliseconds(300));

        private static readonly Regex PlaceholderNameRegex = new("^[0-9a-zA-Z_]+$", RegexOptions.Compiled, RegexConstants.DefaultTimeout);
        private static readonly Regex PlaceholderAlignmentRegex = new("^-?[0-9]+$", RegexOptions.Compiled, RegexConstants.DefaultTimeout);

        public static bool ContainsErrors(string template, out List<ParsingError> errors)
        {
            var result = Helpers.MessageTemplates.Parse(template, TemplateRegex);
            errors = result.Success
                ? result.Placeholders.Select(ParsePlaceholder).Where(x => x is not null).ToList()
                : [new("should be syntactically correct", 0, template.Length)];
            return errors.Count > 0;
        }

        private static ParsingError ParsePlaceholder(Helpers.MessageTemplates.Placeholder placeholder)
        {
            if (placeholder.Length == 0)
            {
                return new($"should not contain empty placeholder", placeholder.Start - 1, 2); // highlight both brackets '{}', since there is no content
            }

            var parts = Split(placeholder.Name);
            if (!PlaceholderNameRegex.SafeIsMatch(parts.Name))
            {
                return new($"placeholder '{parts.Name}' should only contain chars, numbers, and underscore", placeholder);
            }
            else if (parts.Alignment is not null && !PlaceholderAlignmentRegex.SafeIsMatch(parts.Alignment))
            {
                return new($"placeholder '{parts.Name}' should have numeric alignment instead of '{parts.Alignment}'", placeholder);
            }
            else if (parts.Format == string.Empty)
            {
                return new($"placeholder '{parts.Name}' should not have empty format", placeholder);
            }
            else
            {
                return null;
            }
        }

        // pattern is: name[,alignment][:format]
        private static Parts Split(string placeholder)
        {
            var start = placeholder[0] is '@' or '$' ? 1 : 0; // skip prefix

            string name;
            string alignment = null;
            string format = null;

            var formatIndex = placeholder.IndexOf(':');
            var alignmentIndex = placeholder.IndexOf(',');
            if (formatIndex >= 0 && alignmentIndex > formatIndex)
            {
                // example {name:format,alignment}
                //               ^^^^^^^^^^^^^^^^ all of this is format, need to reset alignment
                alignmentIndex = -1;
            }

            if (NotFound(alignmentIndex))
            {
                if (NotFound(formatIndex))
                {
                    name = placeholder.Substring(start);
                }
                else
                {
                    name = placeholder.Substring(start, formatIndex - start);
                    format = IsEmpty(formatIndex) ? string.Empty : placeholder.Substring(formatIndex + 1);
                }
            }
            else
            {
                if (NotFound(formatIndex))
                {
                    name = placeholder.Substring(start, alignmentIndex - start);
                    alignment = IsEmpty(alignmentIndex) ? string.Empty : placeholder.Substring(alignmentIndex + 1);
                }
                else
                {
                    name = placeholder.Substring(start, alignmentIndex - start);
                    alignment = placeholder.Substring(alignmentIndex + 1, formatIndex - alignmentIndex - 1);
                    format = placeholder.Substring(formatIndex + 1);
                }
            }

            return new(name, alignment, format);

            bool NotFound(int index) => index == -1;
            bool IsEmpty(int index) => index == placeholder.Length - 1;
        }

        private sealed record Parts(string Name, string Alignment, string Format);

        public sealed record ParsingError(string Message, int Start, int Length)
        {
            public ParsingError(string message, Helpers.MessageTemplates.Placeholder placeholder)
                : this(message, placeholder.Start, placeholder.Length)
            { }
        }
    }
}
