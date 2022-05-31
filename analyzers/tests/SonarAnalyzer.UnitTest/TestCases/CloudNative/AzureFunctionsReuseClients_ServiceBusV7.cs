﻿namespace FunctionApp1
{
    using System;
    using Microsoft.AspNetCore.Http;
    using Microsoft.AspNetCore.Mvc;
    using Microsoft.Azure.WebJobs;
    using Microsoft.Azure.WebJobs.Extensions.Http;
    using Microsoft.Extensions.Logging;
    using System.Threading.Tasks;
    using Azure.Messaging.ServiceBus;
    using Azure.Messaging.ServiceBus.Administration;

    public static class Function1
    {
        const string sampleUrl = @"http://example.com";

        [FunctionName("DefaultSample")]
        public static async Task<IActionResult> Run(
            [HttpTrigger(AuthorizationLevel.Function, "get", "post", Route = null)] HttpRequest req,
            ILogger log)
        {
            var serviceBus = new ServiceBusClient("connectionString");          // Noncompliant {{Reuse client instances rather than creating new ones with each function invocation.}}
            var admin = new ServiceBusAdministrationClient("connectionString"); // Noncompliant

            return new UnauthorizedResult();
        }
    }
}
