﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Immutable;

namespace Tests.Diagnostics
{
    public record MutableInitializedWithImmutable
    {
        public readonly ISet<string> iSetInitializaedWithImmutableSet = ImmutableHashSet.Create("a", "b");
        public readonly IList<string> iListInitializaedWithImmutableArray = ImmutableArray.Create("a", "b");
        public readonly IList<string> iListInitializaedWithImmutableList = ImmutableList.Create("a", "b");
        public readonly IDictionary<string, string> iDictionaryInitializaedWithImmutableDictionary = ImmutableDictionary.Create<string, string>();
        public readonly IList<string> iList = null;
    }

    public record MutableInitializedWithMutable
    {
        public readonly ISet<string> isetInitializaedWithHashSet = new HashSet<string> { "a", "b" }; // FN
        public readonly IList<string> iListInitializaedWithList = new List<string> { "a", "b" }; // FN
        public readonly IDictionary<string, string> iDictionaryInitializaedWithDictionary = new Dictionary<string, string>(); // FN
    }

    public record MutableInitializedWithMutablePositional(string Property)
    {
        public readonly ISet<string> isetInitializaedWithHashSet = new HashSet<string> { "a", "b" }; // FN
        public readonly IList<string> iListInitializaedWithList = new List<string> { "a", "b" }; // FN
        public readonly IDictionary<string, string> iDictionaryInitializaedWithDictionary = new Dictionary<string, string>(); // FN
    }
}
