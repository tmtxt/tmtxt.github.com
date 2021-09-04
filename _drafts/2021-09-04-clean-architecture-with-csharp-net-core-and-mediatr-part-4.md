---
layout: post
title: "Clean architecture with C#/.Net Core and MediatR - Part 4"
description: ""
categories: [misc]
tags: []
thumbnail:
---

> [Clean architecture with C#/.Net Core and MediatR - Part 3]({%post_url 2021-09-03-clean-architecture-with-csharp-net-core-and-mediatr-part-3%})

# MediatR FTW

Clean Architecture is an Interface design. Everything is connected via Interfaces and their hidden
Implementations. A request comes from the Framework layer to the Business handler via a Business
interface. A request from the Business circle to the database or other services is also activated
using an Adapter interface.

**Mediator** pattern and **MediatR** library are just another way to write Interfaces and
Implementations
(via the `TRequest`/`TResponse` type). In fact, you can simply define your own interface and
attach the corresponding implementation through your IOC container. However, the main reason I use
**MediatR** is because of its excellent dynamic pipeline behavior, which helps me separate most of the
cross-cutting concerns out of the main handler, makes everything cleaner and produces a concise,
testable handler class.

A very simple handler in **MediatR** looks like this

```csharp
public class InsertUser
{
    /// <summary>
    /// Insert a new User and return that User
    /// </summary>
    public class Request : IRequest<Response>
    {
        public int ClientId { get; set; }
        public string Username { get; set; }
        public string Password { get; set; }
    }

    public class Response
    {
        public User User { get; set; }
    }

    public class Handler : IRequestHandler<Request, Response>
    {
        public async Task<Response> Handle(Request request, CancellationToken cancellationToken)
        {
            // implement your logic here
            await CheckExistence(request.UserName);

            // implement your logic here
            var user = await SomeFunction(cancellationToken);

            return new Response
            {
                User = user
            }
        }
    }
}
```

In this simplest form, it doesn't look so different from the way we usually do with a normal
Interface. However, let's imagine what will happen when you want to add these requirements

- Log the related information to debug later.
- Track the process metrics to monitor and analyze performance.
- Lock the process to avoid race-condition.
- Transform the request/response format in a pre-defined way.
- Handle errors.
- Other cross-cutting concerns?...
- A more important question: How to group related requests and re-use these cross-cutting
  concern handlers?

<!-- more -->

# MediatR Request Pipeline - A comprehensible solution

**MediatR Request Pipeline** is just another kind of **Request Middleware**. You simply need to
define the **Behavior** that you want for a type and make
sure your `Request` object follow that type.

Here is an example about how to decouple logging and monitoring logic for the above `Handler`

```csharp
public class LoggingBehavior : IPipelineBehavior<Request, Response>
{
    private readonly ILogger _logger;

    public LoggingBehavior(ILogger logger)
    {
        _logger = logger;
    }

    public async Task<Response> Handle(
        Request request, CancellationToken cancellationToken, RequestHandlerDelegate<Response> next)
    {
        // log the input data
        _logger.Log(LogLevel.Information, "Creating new user...");
        _logger.Log(LogLevel.Information, new { request.Username }.ToString());

        var res = await next();

        // log the created data
        var createdUser = res.User;
        _logger.Log(LogLevel.Information, "New user created!");
        _logger.Log(LogLevel.Information, new { createdUser.Id }.ToString());

        return res;
    }
}
```

and then register it with your DI Container (**Autofac** in this case)

```csharp
public class AutofacModule : Module
{
    protected override void Load(ContainerBuilder builder)
    {
        builder.RegisterType<LoggingBehavior>().As<IPipelineBehavior<Request, Response>>();
    }
}
```

As you can see, the main `Handler` class doesn't change at all. We don't need to update any test case
to adapt with this new change. You don't need to prepare or mock anything related to your Logging
and Monitoring service. You can focus on testing only the main Business logic. All the
cross-cutting concerns could be easily abstracted away.

# Group related Requests and Reuse Behaviors

In case you want to group related Requests and Reuse the existing Behaviors, all you need to do is
to define an Interface for that group and add a generic Behavior for that type. Let's say that you
want to track the processing time for each different client to monitor which one causes performance
problem for your system, here is how

```csharp
public interface IWithMonitoring
{
    int ClientId { get; }
}

public class MonitoringBehavior<TRequest, TResponse> : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IWithMonitoring
{
    private readonly ILogger _logger;

    public MonitoringBehavior(ILogger logger)
    {
        _logger = logger;
    }

    public async Task<TResponse> Handle(
        TRequest request, CancellationToken cancellationToken, RequestHandlerDelegate<TResponse> next)
    {
        // measure the processing time
        var stopWatch = new Stopwatch();
        stopWatch.Start();

        var response = await next();

        stopWatch.Stop();
        var data = $"Client {request.ClientId} - Request {nameof(TRequest)} - Time: {stopWatch.ElapsedMilliseconds}";
        _logger.Log(LogLevel.Information, data);

        return response;
    }
}

// Make sure your Request class satisfy the type
public class Request : IRequest<Response>, IWithMonitoring
{
    // ...
}
```

and of course, register the generic type with your IOC container

```csharp
public class AutofacModule : Module
{
    protected override void Load(ContainerBuilder builder)
    {
        builder.RegisterGeneric(typeof(MonitoringBehavior<,>)).As(typeof(IPipelineBehavior<,>));
    }
}
```

Done, everything is applied automagically!

# Limitations

The biggest disadvantage I have with MediatR is the lack of circular-dependencies protection. The
easiest workaround is to structure your application following a multiple level design, where each
Handler can only activate the Handlers in the lower level. This is achieved by coding convention and
code reviewing.

![Multi Level](/files/2021-07-13-clean-architecture/multi-level.png)
