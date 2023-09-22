﻿using System.Text;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis.Text;
using SonarAnalyzer.Protobuf;
using SonarAnalyzer.Rules;
using static SonarAnalyzer.Rules.CSharp.TokenTypeAnalyzer;
using Match = System.Text.RegularExpressions.Match;

namespace SonarAnalyzer.UnitTest.Rules;

public partial class TokenTypeAnalyzerTest
{
    private static readonly Dictionary<TokenType, string> TokenTypeAcronyms = new()
    {
        { TokenType.Keyword, "k" },
        { TokenType.NumericLiteral, "n" },
        { TokenType.StringLiteral, "s" },
        { TokenType.TypeName, "t" },
        { TokenType.Comment, "c" },
        { TokenType.UnknownTokentype, "u" },
    };

    private static class ClassifierTestHarness
    {
        private const int TokenAnnotationChars = 4; // [u:]
        private const int PrefixTokenAnnotationChars = 3; // [u:
        private static readonly Regex TokenTypeRegEx = new(TokenGroups(TokenTypeAcronyms.Select(x => TokenGroup(x.Key, x.Value)).ToArray()));

        public static void AssertTokenTypes(string code, bool allowSemanticModel = true, bool ignoreCompilationErrors = false)
        {
            var (tree, model, expectedTokens) = ParseTokens(code, ignoreCompilationErrors);
            model = allowSemanticModel ? model : null; // The TokenClassifier will throw if the semantic model is used.
            // filePath for the snippet is defined later by TestHelper.CompileCS -> ignore filePath when classifying tokens and trivia
            var tokenClassifier = new TokenClassifier(model, false, string.Empty, ImmutableSortedSet<LineDirectiveEntry>.Empty);
            var triviaClassifier = new TriviaClassifier(string.Empty, ImmutableSortedSet<LineDirectiveEntry>.Empty);
            expectedTokens.Should().SatisfyRespectively(expectedTokens.Select(
                (Func<ExpectedToken, Action<ExpectedToken>>)(_ => token => CheckClassifiedToken(tokenClassifier, triviaClassifier, tree, token))));
        }

        private static void CheckClassifiedToken(TokenClassifier tokenClassifier, TriviaClassifier triviaClassifier, SyntaxTree tree, ExpectedToken expected)
        {
            var expectedLineSpan = tree.GetLocation(expected.Position).GetLineSpan();
            var because = $$"""token with text "{{expected.TokenText}}" at position {{expectedLineSpan}} was marked as {{expected.TokenType}}""";
            var (actualLocation, classification) = FindActual(tokenClassifier, triviaClassifier, tree, expected, because);
            if (classification == null)
            {
                because = $$"""classification for token with text "{{expected.TokenText}}" at position {{expectedLineSpan}} is null""";
                expected.TokenType.Should().Be(TokenType.UnknownTokentype, because);
                actualLocation.SourceSpan.Should().Be(expected.Position, because);
            }
            else
            {
                classification.Should().Be(new TokenTypeInfo.Types.TokenInfo
                {
                    TokenType = expected.TokenType,
                    TextRange = new TextRange
                    {
                        StartLine = expectedLineSpan.StartLinePosition.Line + 1,
                        StartOffset = expectedLineSpan.StartLinePosition.Character,
                        EndLine = expectedLineSpan.EndLinePosition.Line + 1,
                        EndOffset = expectedLineSpan.EndLinePosition.Character,
                    },
                }, because);
            }
        }

        private static (Location Location, TokenTypeInfo.Types.TokenInfo TokenInfo) FindActual(TokenClassifier tokenClassifier, TriviaClassifier triviaClassifier, SyntaxTree tree, ExpectedToken expected, string because)
        {
            if (expected.TokenType == TokenType.Comment)
            {
                var trivia = tree.GetRoot().FindTrivia(expected.Position.Start);
                return (tree.GetLocation(trivia.FullSpan), triviaClassifier.ClassifyTrivia(trivia));
            }
            else
            {
                var token = tree.GetRoot().FindToken(expected.Position.Start);
                var f = () => tokenClassifier.ClassifyToken(token);
                var tokenInfo = f.Should().NotThrow($"semanticModel should not be queried for {because}").Which;
                return (token.GetLocation(), tokenInfo);
            }
        }

        private static (SyntaxTree Tree, SemanticModel Model, IReadOnlyCollection<ExpectedToken> ExpectedTokens) ParseTokens(string code, bool ignoreCompilationErrors = false)
        {
            var matches = TokenTypeRegEx.Matches(code);
            var sb = new StringBuilder(code.Length);
            var expectedTokens = new List<ExpectedToken>(matches.Count);
            var lastMatchEnd = 0;
            var match = 0;
            foreach (var group in matches.Cast<Match>().Select(m => m.Groups.Cast<Group>().First(g => g.Success && g.Name != "0")))
            {
                var expectedTokenType = (TokenType)Enum.Parse(typeof(TokenType), group.Name);
                var position = group.Index - (match * TokenAnnotationChars);
                var length = group.Length - TokenAnnotationChars;
                var tokenText = group.Value.Substring(PrefixTokenAnnotationChars, group.Value.Length - TokenAnnotationChars);
                expectedTokens.Add(new ExpectedToken(expectedTokenType, tokenText, new TextSpan(position, length)));

                sb.Append(code.Substring(lastMatchEnd, group.Index - lastMatchEnd));
                sb.Append(tokenText);
                lastMatchEnd = group.Index + group.Length;
                match++;
            }
            sb.Append(code.Substring(lastMatchEnd));
            var (tree, model) = ignoreCompilationErrors ? TestHelper.CompileIgnoreErrorsCS(sb.ToString()) : TestHelper.CompileCS(sb.ToString());
            return (tree, model, expectedTokens);
        }

        private static string TokenGroups(params string[] groups) =>
            string.Join("|", groups);

        private static string TokenGroup(TokenType tokenType, string shortName) =>
            $$"""(?'{{tokenType}}'\[{{shortName}}\:[^\]]+\])""";

        private readonly record struct ExpectedToken(TokenType TokenType, string TokenText, TextSpan Position);
    }
}
