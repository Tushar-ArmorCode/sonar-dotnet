﻿var a1 = false;
if (a1)             // Noncompliant
{
    DoSomething();  // never executed
}

void DoSomething() { }
