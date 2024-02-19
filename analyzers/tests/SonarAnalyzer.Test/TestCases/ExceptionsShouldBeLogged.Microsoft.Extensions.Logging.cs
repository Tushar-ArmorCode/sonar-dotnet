﻿using System;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;

// Logging methods from: https://learn.microsoft.com/en-us/dotnet/api/microsoft.extensions.logging.loggerextensions?view=dotnet-plat-ext-8.0
public class TestCases
{
    private readonly ILogger logger = new Logger<TestCases>(new NullLoggerFactory());

    public void Simple()
    {
        try { }
        catch (Exception e)
        {
            logger.Log(LogLevel.Critical, e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.Log(LogLevel.Critical, "Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.Log(LogLevel.Critical, new EventId(1), e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.Log(LogLevel.Critical, new EventId(1), "Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogCritical(e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogCritical("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogCritical(new EventId(1), e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogCritical("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogDebug(e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogDebug("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogDebug(new EventId(1), e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogDebug("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogError(e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogError("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogError(new EventId(1), e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogError("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogInformation(e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogInformation("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogInformation(new EventId(1), e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogInformation("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogTrace(e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogTrace("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogTrace(new EventId(1), e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogTrace("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogWarning(e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogWarning("Message!"); // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            logger.LogWarning(new EventId(1), e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogWarning("Message!"); // Noncompliant
        }
    }

    public void MultipleLogsInTheSameCatch()
    {
        try { }
        catch (Exception e)
        {
            logger.LogWarning(new EventId(1), e, "Message!");
            logger.LogWarning(new EventId(1), e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogWarning(new EventId(1), "Message!");
            logger.LogWarning(new EventId(1), e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            logger.LogWarning(new EventId(1), "Message!"); // Noncompliant
            logger.LogWarning(new EventId(1), "Message!"); // Noncompliant
        }
    }

    public void UsageAsStaticMethod()
    {
        try { }
        catch (Exception e)
        {
            LoggerExtensions.Log(logger, LogLevel.Critical, "Message!");    // Noncompliant
            LoggerExtensions.LogCritical(logger, "Message!");               // Noncompliant
            LoggerExtensions.LogDebug(logger, "Message!");                  // Noncompliant
            LoggerExtensions.LogError(logger, "Message!");                  // Noncompliant
            LoggerExtensions.LogInformation(logger, "Message!");            // Noncompliant
            LoggerExtensions.LogTrace(logger, "Message!");                  // Noncompliant
            LoggerExtensions.LogWarning(logger, "Message!");                // Noncompliant
        }

        try { }
        catch (Exception e)
        {
            LoggerExtensions.Log(logger, LogLevel.Critical, e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            LoggerExtensions.LogCritical(logger, e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            LoggerExtensions.LogDebug(logger, e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            LoggerExtensions.LogError(logger, e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            LoggerExtensions.LogInformation(logger, e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            LoggerExtensions.LogTrace(logger, e, "Message!");
        }

        try { }
        catch (Exception e)
        {
            LoggerExtensions.LogWarning(logger, e, "Message!");
        }
    }

    public void NoLogsInCatch()
    {
        try { }
        catch (Exception e)
        {
        }
    }

    public void CallingMethodToLog()
    {
        try { }
        catch (Exception e)
        {
            Log();
        }
    }

    private void Log()
    {
        logger.LogCritical("Message!"); // Compliant - we do not check this
    }

    private void LogFromMultipleCatchBlocks()
    {
        try { }
        catch (DivideByZeroException)
        {
            LoggerExtensions.LogCritical(logger, "Message!"); // Noncompliant
        }
        catch (AggregateException)
        {
            LoggerExtensions.LogCritical(logger, "Message!"); // Noncompliant
        }
        catch (ApplicationException e)
        {
            LoggerExtensions.LogCritical(logger, e, "Message!");
        }
    }

    private void LogFromNestedCatchBlocks()
    {
        try { }
        catch (Exception e)
        {
            logger.LogWarning("Message!");              // Noncompliant
            try { }
            catch (DivideByZeroException)
            {
                logger.LogCritical("Message!");         // Noncompliant
            }
            catch (AggregateException e1)
            {
                logger.LogCritical(e, "Message!");
            }
            catch (Exception e2)
            {
                logger.LogCritical(e2, "Message!");
            }
        }

        try { }
        catch (Exception e)
        {
            try { }
            catch (Exception e2)
            {
                logger.LogCritical(e2, "Message!");    // Noncompliant - Wrong exception
            }
        }
    }

    public void LogFromCatchBlockWithNoException()
    {
        try { }
        catch
        {
            logger.LogCritical("Message!");           // Noncompliant
        }
    }

    public void LogFromIfStatement()
    {
        try { }
        catch (DivideByZeroException e)
        {
            if (true)
            {
                logger.LogCritical("Message!");       // Noncompliant
            }
        }
        catch (Exception e)
        {
            if (true)
            {
                logger.LogCritical(e, "Message!");
            }
        }
    }

    public void LogFromCustomLogger()
    {
        try { }
        catch
        {
            new CustomLogger().LogCritical("Message!");
        }
    }

    public class CustomLogger
    {
        public void LogCritical(string message) { }
    }
}
