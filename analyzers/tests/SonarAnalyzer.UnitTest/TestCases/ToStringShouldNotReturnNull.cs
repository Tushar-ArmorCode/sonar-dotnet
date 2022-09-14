﻿public static class Condition
{
    public static bool When()
    {
        return true;
    }
}

namespace Compliant
{
    class OtherMethodReturnsNullString
    {
        string Returns()
        {
            return null;
        }
    }

    class ReturnsSomeString
    {
        public override string ToString()
        {
            if (Condition.When())
            {
                return "Hello, world!";
            }
            return "Hello, world!";
        }
    }

    class ReturnsEmptyString
    {
        public override string ToString()
        {
            if (Condition.When())
            {
                return "";
            }
            return "";
        }
    }

    class ReturnsStringEmpty
    {
        public override string ToString()
        {
            if (Condition.When())
            {
                return string.Empty;
            }
            return string.Empty;
        }
    }

    struct ReturnsStringEmptyStruct
    {
        public override string ToString()
        {
            if (Condition.When()) { return string.Empty; }
            return string.Empty;
        }
    }
}

class Noncompliant
{
    public class ReturnsNull
    {
        public override string ToString()
        {
            if (Condition.When())
            {
                return null; // Noncompliant
            //  ^^^^^^^^^^^^
            }
            return null; // Noncompliant {{Return an empty string instead.}}
        }
    }

    public class ReturnsNullViaExpressionBody
    {
        public override string ToString() => null; // Noncompliant
    }

    public class ReturnsNullViaTernaryExpressionBody
    {
        public override string ToString() => Condition.When() ? null : string.Empty; // Noncompliant
    }

    public class ReturnsNullViaTernary
    {
        public override string ToString()
        {
            return Condition.When() ? null : ""; // Noncompliant
        }
    }

    struct StructReturnsNull
    {
        public override string ToString()
        {
            if (Condition.When()) { return null; } // Noncompliant
            return null; // Noncompliant
        }
    }
}
