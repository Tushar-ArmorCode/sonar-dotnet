﻿using System;
using System.Collections.Generic;
using System.Linq.Expressions;

namespace Tests.Diagnostics
{
    public static class RedundantArgument
    {
        public static void M(int x, int y = 5, int z = 7) { /* ... */ }
        public static void M2(int x, int y, int z) { /* ... */ }
        public static void M3(int x = 1, int y = 5, int z = 7) { /* ... */ }
        public static void Ext(this int self, int y = 5, params int[] parameters) { }
        public static void Ext2(this int self, int y, params int[] parameters) { }

        public static void Test()
        {
            var x = "".ToString();
            M(1, 5); //Fixed
            M(1, z: 7); //Fixed
            M(1, 5, // Fixed
                7); // Fixed
            M(1);
            M(1, 2, 4);
            M2(1, 1, 1);
            5.Ext(5); //Fixed
            5.Ext2(5);

            RedundantArgument.Ext(5, parameters: new[] { 4, 4, 5, 6 }); //Fixed
            RedundantArgument.Ext(5, y: 5, parameters: new int[] { 4, 4, 5, 6 }); //Fixed
            RedundantArgument.Ext(5, 5); //Fixed

            M3(1,//Fixed
                y: 5,//Fixed
                z: 7);//Fixed

            M3(1, y: 4); //Fixed
            M3(x: 1, y: 4); //Fixed

            M3(y: 4); //Fixed
        }
    }

    // Issue #789: Cannot use optional arguments when using expression trees (CS0584)
    public class RedundantArgsInExpressionTrees
    {
        private static string FuncWithOptionals(string str = null, params string[] args)
        {
            return str;
        }

        // Field declaration -> variable declaration
        Func<string> normalField = () => FuncWithOptionals(args: new[] { "111", "222" }); //Fixed
        readonly Expression<Action> expTreeField = () => FuncWithOptionals(null); //Compliant - expression tree, so cannot use defaults

        // Property declaration
        Func<string> normalProperty => () => FuncWithOptionals(str: null); //Fixed
        Expression<Action> expTreeProperty => () => FuncWithOptionals(null); //Compliant

        void takeExpression(Expression<Func<int, int>> expr) { }

        public static void Method1()
        {
            // Variable declaration
            Func<string> var1 = () => FuncWithOptionals(null); //Fixed
            Expression<Action> expTreeVar = () => FuncWithOptionals(null); //Compliant
            takeExpression(() => FuncWithOptionals(null)); //Compliant

            // Simple assigment
            var1 = () => FuncWithOptionals(str: null, args: "123"); //Fixed
            expTreeVar = () => FuncWithOptionals(null); //Compliant
        }
    }
}
