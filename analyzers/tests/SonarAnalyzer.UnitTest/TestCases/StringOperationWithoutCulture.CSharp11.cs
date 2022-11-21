﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Globalization;
using System.Linq.Expressions;

namespace Tests.Diagnostics
{
    public class StringOperationWithoutCulture
    {
        void TestInterpolatedStrings()
        {
            int foo = 10;
            string s = $"Hello {foo switch
                {
                    > 0 => "FOO",
                    _ => "Bar",
                }}".ToUpper(); // Noncompliant {{Define the locale to be used in this string operation.}}
        }
    }
}
