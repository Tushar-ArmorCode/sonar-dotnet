﻿using System;

namespace Tests.Diagnostics
{
    public struct BarStruct
    {
        public int someField = 0; // Noncompliant FP for versions < C# 11 there's no default values for fields and the user needs to set it explicitly.
        public BarStruct()
        { }
    }
}

