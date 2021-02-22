﻿using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

namespace Tests.TestCases
{
    internal class C
    {
        private static Dictionary<string, List<int>> Dict;
    }

    class StaticFieldInGenericClass<T/*comment*/, /*comment*/U>
        where T : class
    {
        private static readonly ConditionalWeakTable<T, Task<T>>.CreateValueCallback s_taskCreationCallback = Task.FromResult<T>;

        private static Dictionary<string, List<T>> Dict, Dict4 = new Dictionary<string,List<T>>();
        private static Dictionary<string, T[]> Dict2;

        public static string sProp1 { get; set; } //Noncompliant
//                           ^^^^^^
        public /*comment */static string sProp2 { get; set; } //Noncompliant {{A static field in a generic type is not shared among instances of different close constructed types.}}
        public string sProp3 { get; set; }

        public static T tProp { get; set; }

        internal static string sField; //Noncompliant

        internal string NestedClassUsage => NestedClass.StringField;

        //https://github.com/SonarSource/sonar-dotnet/issues/4081
        private static class NestedClass
        {
            public static readonly string StringField = "String"; // FN
        }
    }
}
