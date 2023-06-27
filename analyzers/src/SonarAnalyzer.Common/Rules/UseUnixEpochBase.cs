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

namespace SonarAnalyzer.Rules;

public abstract class UseUnixEpochBase<TSyntaxKind, TLiteralExpression, TMemberAccessExpression> : SonarDiagnosticAnalyzer<TSyntaxKind>
    where TSyntaxKind : struct
    where TLiteralExpression : SyntaxNode
    where TMemberAccessExpression : SyntaxNode
{
    private const string DiagnosticId = "S6588";

    protected override string MessageFormat => "Use \"{0}.UnixEpoch\" instead of creating {0} instances that point to the unix epoch time";

    protected abstract bool IsDateTimeKindUtc(TMemberAccessExpression memberAccess);
    protected abstract bool IsGregorianCalendar(SyntaxNode node);
    protected abstract bool IsZeroTimeOffset(SyntaxNode node);

    protected UseUnixEpochBase() : base(DiagnosticId) { }

    protected sealed override void Initialize(SonarAnalysisContext context) =>
        context.RegisterCompilationStartAction(start =>
        {
            if (!CompilationTargetsValidNetVersion(start.Compilation))
            {
                return;
            }

            context.RegisterNodeAction(
                Language.GeneratedCodeRecognizer,
                c =>
                {
                    var argumentCount = Language.Syntax.ArgumentExpressions(c.Node).Count();
                    if (argumentCount < 3)
                    {
                        return;
                    }

                    var type = c.SemanticModel.GetTypeInfo(c.Node).Type;
                    if (type.DerivesFrom(KnownType.System_DateTime) && IsEpochCtor(c.Node, c.SemanticModel))
                    {
                        c.ReportIssue(Diagnostic.Create(Rule, c.Node.GetLocation(), "DateTime"));
                    }
                    else if (type.DerivesFrom(KnownType.System_DateTimeOffset) && IsEpochCtor(c.Node, c.SemanticModel))
                    {
                        c.ReportIssue(Diagnostic.Create(Rule, c.Node.GetLocation(), "DateTimeOffset"));
                    }
                },
                Language.SyntaxKind.ObjectCreationExpressions);
        });

    protected static bool IsValueEqualsTo(TLiteralExpression literal, int value) =>
        literal.ChildTokens().First().ValueText == value.ToString();

    private bool IsEpochCtor(SyntaxNode node, SemanticModel model)
    {
        var methodSymbol = (IMethodSymbol)model.GetSymbolInfo(node).Symbol;
        var lookup = Language.MethodParameterLookup(node, methodSymbol);

        if (IsParameterExistingAndLiteralEqualTo("year", 1970, lookup)
            && IsParameterExistingAndLiteralEqualTo("month", 1, lookup)
            && IsParameterExistingAndLiteralEqualTo("day", 1, lookup)
            && IsParameterNonExistingOrLiteralEqualTo("hour", 0, lookup)
            && IsParameterNonExistingOrLiteralEqualTo("minute", 0, lookup)
            && IsParameterNonExistingOrLiteralEqualTo("second", 0, lookup)
            && IsParameterNonExistingOrLiteralEqualTo("millisecond", 0, lookup)
            && IsParameterNonExistingOrLiteralEqualTo("microsecond", 0, lookup)
            && IsDateTimeKindNonExistingOrUtc(lookup)
            && IsCalendarNonExistingOrGregorian(lookup)
            && IsOffsetNonExistingOrZero(lookup))
        {
            return true;
        }
        return false;
    }

    private static bool IsParameterExistingAndLiteralEqualTo(string parameterName, int value, IMethodParameterLookup lookup) =>
        lookup.TryGetSyntax(parameterName, out var expressions) && IsLiteralAndEqualTo(expressions[0], value);

    private static bool IsParameterNonExistingOrLiteralEqualTo(string parameterName, int value, IMethodParameterLookup lookup) =>
        !lookup.TryGetSyntax(parameterName, out var expressions) || IsLiteralAndEqualTo(expressions[0], value);

    private static bool IsLiteralAndEqualTo(SyntaxNode node, int value) =>
        node is TLiteralExpression literal && IsValueEqualsTo(literal, value);

    private bool IsDateTimeKindNonExistingOrUtc(IMethodParameterLookup lookup) =>
        !lookup.TryGetSyntax("kind", out var expressions)
        || (expressions[0] is TMemberAccessExpression memberAccess && IsDateTimeKindUtc(memberAccess));

    private bool IsCalendarNonExistingOrGregorian(IMethodParameterLookup lookup) =>
        !lookup.TryGetSyntax("calendar", out var expressions) || IsGregorianCalendar(expressions[0]);

    private bool IsOffsetNonExistingOrZero(IMethodParameterLookup lookup) =>
        !lookup.TryGetSyntax("offset", out var expressions) || IsZeroTimeOffset(expressions[0]);

    private static bool CompilationTargetsValidNetVersion(Compilation compilation) =>
        compilation.GetTypeByMetadataName(KnownType.System_DateTime) is var dateType && dateType.GetMembers("UnixEpoch").Any();
}
