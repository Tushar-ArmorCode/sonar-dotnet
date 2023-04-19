﻿using Microsoft.VisualStudio.TestTools.UnitTesting;
using NUnit.Framework;
using System.Threading;
using Xunit;

class Compliant
{
    [Test]
    public void TestWithoutThreadSleep()
    {
        Xunit.Assert.Equal("42", "The answer to life, the universe, and everything");
    }

    [Test]
    public void CallToOtherSleepMethod()
    {
        Sleep(42);
    }

    void ThreadSleepNotInTest()
    {
        Thread.Sleep(42);
    }

    private void Sleep(int durartion) { }
}

class NonCompliant
{
    [Test]
    public void ThreadSleepInNUnitTest()
    {
        Thread.Sleep(42); // {{Do not use 'Thread.Sleep()' in a test.}}
//      ^^^^^^^^^^^^^^^^
    }

    [Fact]
    public void ThreadSleepInXUnitTest()
    {
        Thread.Sleep(42); // Noncompliant
    }

    [TestMethod]
    public void ThreadSleepInMSTest()
    {
        Thread.Sleep(42); // Noncompliant
    }
}
