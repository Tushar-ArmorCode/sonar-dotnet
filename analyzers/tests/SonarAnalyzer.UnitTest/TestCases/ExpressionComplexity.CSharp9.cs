﻿var d1 = true && false && true && false && true && true; // Noncompliant

object x = null;

if (x is true or true or true or true or true) { } // Noncompliant

if (x is true and true and true and true and true) { } // Noncompliant

if (x is 10 or 20 or 40 or 50 or 60 or 70) { } // Noncompliant

if (x is < 10 or < 20 or < 30 and (40 or 50 or 60)) { } // Noncompliant
