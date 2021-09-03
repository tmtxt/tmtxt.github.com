---
layout: post
title: "Clean architecture with C#/.Net Core and MediatR - Part 3"
description: ""
categories: []
tags: []
thumbnail:
---

> [Clean architecture with C#/.Net Core and MediatR - Part 2]({%post_url 2021-09-02-clean-architecture-with-csharp-net-core-and-mediatr-part-2%})

# 3. Runtime Layer

The **Runtime Layer** contains nearly no logic. It is simply a place to bootstrap the application and
register all the necessary dependencies. It acts as a gateway for inputting the data to the Business
Flows and transfer the output back to the caller. That means, your Business Flows can be embedded
into any Runtime type, from an HTTP API Server to a Worker that processes data from a Message
Queues or a onetime Script,... Here are some examples how they should look like

## HTTP Server

![Http Runtime](/files/2021-07-13-clean-architecture/http.png)

For HTTP Server, the APIs simply transform the data from the deserializable format (the HTTP
Request) to the Business Flow input and then serialize the output to send back to the client.

In case you use **ASP.Net Core** and **Autofac** (like me)...

```csharp
public class Startup
{
    // ...other methods

    /// <summary>
    /// Autofac method
    /// </summary>
    /// <param name="builder"></param>
    public void ConfigureContainer(ContainerBuilder builder)
    {
        builder.RegisterModule<Truongtx.Business.AutofacModule>();
        builder.RegisterModule<Truongtx.Adapter.AutofacModule>();
    }
}

[ApiController]
public class NpsController : ControllerBase
{
    private readonly Business.ISendMarketingEmails _sendMarketingEmails;

    public NpsController(Business.ISendMarketingEmails sendMarketingEmails)
    {
        _sendMarketingEmails = sendMarketingEmails;
    }

    /// <summary>
    /// Send Marketing Emails for a Campaign
    /// </summary>
    /// <param name="marketingCampaignId"></param>
    /// <returns></returns>
    [Route("/api/marketing-campaigns/{marketingCampaignId}/send-emails")]
    [HttpPost]
    public Task<string> SendMarketingEmails(int marketingCampaignId)
        => _sendMarketingEmails.Execute(marketingCampaignId);

    // ... other APIs
}
```

<!-- more -->

## Message Queue worker

![Worker Runtime](/files/2021-07-13-clean-architecture/message-bus.png)

Similar to an HTTP Server, instead of receiving the request from web browser, the Worker will pull
the Messages from a Message Queue and then input to the Business Flows.

```csharp
class Program
{
    static async Task Main(string[] args)
    {
        var builder = new ContainerBuilder();
        builder.RegisterModule<Business.AutofacModule>();
        builder.RegisterModule<Adapter.AutofacModule>();
        IContainer container = builder.Build();

        using (var scope = container.BeginLifetimeScope())
        {
            var client = scope.Resolve<IMessageQueueClient>();
            var message = await client.PullMessage();

            var mapper = scope.Resolve<IMapper>();
            var handler = scope.Resolve<ISendMarketingEmails>();
            await handler.Execute(mapper.Map<BusinessInput>(message));
        }
    }
}
```

# To be continued...
