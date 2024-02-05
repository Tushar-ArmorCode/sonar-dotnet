﻿using SonarAnalyzer.Test;
using SonarAnalyzer.Test.Helpers;

namespace ITs.JsonParser.Test;

[TestClass]
public class IssueParserTest
{
    public TestContext TestContext { get; set; }

    [TestMethod]
    public void Execute_Single()
    {
        var root = TestDirectory();
        var inputPath = Path.Combine(root, "read");
        var outputPath = Path.Combine(root, "write");
        Directory.CreateDirectory(Path.Combine(inputPath, "solution"));
        Sarif.CreateReport(inputPath, "solution", "project", "net6.0", Sarif.CreateIssue("S100", "Message_1", "foo/bar/File1.cs", 1, 1));
        var outFile = Path.Combine(outputPath, "solution", "S100-project-net6.0.json");

        IssueParser.Execute(inputPath, outputPath);

        VerifyResultFile(outFile, """
            {
              "Issues": [
                {
                  "Id": "S100",
                  "Message": "Message_1",
                  "Location": {
                    "Uri": "https://github.com/SonarSource/sonar-dotnet/blob/master/analyzers/its/foo/bar/File1.cs#L1",
                    "Region": {
                      "StartLine": 1,
                      "StartColumn": 42,
                      "EndLine": 1,
                      "EndColumn": 99
                    }
                  }
                }
              ]
            }
            """);
    }

    [TestMethod]
    public void Execute_Multiple()
    {
        var root = TestDirectory();
        var inputPath = Path.Combine(root, "read");
        var outputPath = Path.Combine(root, "write");
        Directory.CreateDirectory(Path.Combine(inputPath, "solution1"));
        Directory.CreateDirectory(Path.Combine(inputPath, "solution2"));

        // solution1/project1-net6.0.json
        Sarif.CreateReport(
            inputPath,
            "solution1",
            "project1",
            "net6.0",
            Sarif.CreateIssue("S100", "Message_1", "foo/bar/File1.cs", 1, 1),
            Sarif.CreateIssue("S100", "Message_2", "foo/bar/File1.cs", 42, 43),  // #L1-L2 location range
            Sarif.CreateIssue("S142", "Message_3"));                             // null location

        // solution1/project2-net6.0.json
        Sarif.CreateReport(
            inputPath,
            "solution1",
            "project2",
            null, // No target framework specified
            Sarif.CreateIssue("S200", "Message_1", "foo/bar/File1.cs", 0, 0));

        // solution2/project-net6.0.json
        Sarif.CreateReport(
            inputPath,
            "solution2",
            "project",
            "net6.0",
            Sarif.CreateIssue("S100", "Message_1", "foo/bar/File1.cs", 1, 1));

        var outFile1 = Path.Combine(outputPath, "solution1", "S100-project1-net6.0.json");
        var outFile2 = Path.Combine(outputPath, "solution1", "S142-project1-net6.0.json");
        var outFile3 = Path.Combine(outputPath, "solution1", "S200-project2.json");
        var outFile4 = Path.Combine(outputPath, "solution2", "S100-project-net6.0.json");

        IssueParser.Execute(inputPath, outputPath);

        VerifyResultFile(outFile1, """
            {
              "Issues": [
                {
                  "Id": "S100",
                  "Message": "Message_1",
                  "Location": {
                    "Uri": "https://github.com/SonarSource/sonar-dotnet/blob/master/analyzers/its/foo/bar/File1.cs#L1",
                    "Region": {
                      "StartLine": 1,
                      "StartColumn": 42,
                      "EndLine": 1,
                      "EndColumn": 99
                    }
                  }
                },
                {
                  "Id": "S100",
                  "Message": "Message_2",
                  "Location": {
                    "Uri": "https://github.com/SonarSource/sonar-dotnet/blob/master/analyzers/its/foo/bar/File1.cs#L42-L43",
                    "Region": {
                      "StartLine": 42,
                      "StartColumn": 42,
                      "EndLine": 43,
                      "EndColumn": 99
                    }
                  }
                }
              ]
            }
            """);

        VerifyResultFile(outFile2, """
            {
              "Issues": [
                {
                  "Id": "S142",
                  "Message": "Message_3",
                  "Location": {
                    "Uri": null,
                    "Region": null
                  }
                }
              ]
            }
            """);

        VerifyResultFile(outFile3, """
            {
              "Issues": [
                {
                  "Id": "S200",
                  "Message": "Message_1",
                  "Location": {
                    "Uri": "https://github.com/SonarSource/sonar-dotnet/blob/master/analyzers/its/foo/bar/File1.cs#L0",
                    "Region": {
                      "StartLine": 0,
                      "StartColumn": 42,
                      "EndLine": 0,
                      "EndColumn": 99
                    }
                  }
                }
              ]
            }
            """);

        VerifyResultFile(outFile4, """
            {
              "Issues": [
                {
                  "Id": "S100",
                  "Message": "Message_1",
                  "Location": {
                    "Uri": "https://github.com/SonarSource/sonar-dotnet/blob/master/analyzers/its/foo/bar/File1.cs#L1",
                    "Region": {
                      "StartLine": 1,
                      "StartColumn": 42,
                      "EndLine": 1,
                      "EndColumn": 99
                    }
                  }
                }
              ]
            }
            """);
    }

    private string TestDirectory() =>
        Path.GetDirectoryName(TestHelper.TestPath(TestContext, "unused"));

    private static void VerifyResultFile(string path, string expected)
    {
        File.Exists(path).Should().BeTrue();
        var result = File.ReadAllText(path);
        Console.WriteLine($"Path: {path}");
        Console.WriteLine(result);
        result.Should().BeIgnoringLineEndings(expected);
    }
}
