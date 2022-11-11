﻿using System;
using System.IO;
using System.Threading.Tasks;

namespace Tests.Diagnostics
{
    public class InvalidCases
    {
        public static async Task<string> FooAsync(string something) // FN
        {
            ArgumentNullException.ThrowIfNull(something); // FN sec

            await Task.Delay(1);
            return something + "foo";
        }
    }

    public class ValidCases
    {
        public static Task FooAsync(string something)
        {
            ArgumentNullException.ThrowIfNull(something);

            return FooInternalAsync(something);
        }

        private static async Task<string> FooInternalAsync(string something)
        {
            await Task.Delay(1);
            return something + "foo";
        }
    }
}
