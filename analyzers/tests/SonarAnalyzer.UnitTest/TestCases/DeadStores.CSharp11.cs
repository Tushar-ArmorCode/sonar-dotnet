﻿using System;


string x = "";   // FN
x = """Test2"""; // FN
Foo(x);

void RawStringLiterals(string param)
{
    param = """Test""";      // Noncompliant

    string x = ""; // Compliant, ignored value
    x = """Test2""";
    Foo(x);

    string y = """Test1"""; // Noncompliant
    y = """Test2""";
    Foo(y);
}

void MultilineRawStringLiterals(string param)
{
    param = """ 
        This
        is
        multiline
        """; // Noncompliant@-4

    string x = """

        """; // Compliant (empty multi-line)
    x = """
        Something
        """;

    string z = """
        Something
        """; // Noncompliant@-2
    z = """
        
        """;

    string y = """
        This
        is
        multiline
        """; // Noncompliant@-4
    y = """
        This
        is
        also
        multiline
        """;

    Foo(x);
    Foo(y);
    Foo(z);
}

void InterpolatedRawStringLiterals(string param)
{
    string aux = """Test""";
    string auxMultiline = """
        This
        is
        multiline
        """;

    param = $"""{aux}Test""";      // Noncompliant
    param = $"""{auxMultiline}Test""";      // Noncompliant
    param = $"""
        {aux}
        Test
        """;      // Noncompliant@-3
    param = $"""
        {auxMultiline}
        Test
        """;      // Noncompliant@-3

    string empty = "";
    string x = $"""{empty}"""; // Noncompliant FP (string is still empty, should be compliant)
    x = $"""{empty}Test""";
    Foo(x);

    string emptyMultiline = """

        """;
    string q = $"""{emptyMultiline}"""; // Noncompliant FP (string is still empty, should be compliant)
    q = $"""{emptyMultiline}Test""";
    Foo(q);

    string y = $"""
        Test1{aux}
        """; // Noncompliant@-2
    y = $"""
        Test2{aux}
        """;
    Foo(y);
}

static void Foo(object x){ }
