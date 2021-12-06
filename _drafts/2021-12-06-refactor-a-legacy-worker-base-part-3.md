---
layout: post
title: "Refactor part 3"
description: ""
categories: [.net,javascript]
tags: []
thumbnail:
---

> Part 2: [Refactor a legacy Worker Base - Part 2 - Scope Management]({% post_url 2021-12-06-refactor-a-legacy-worker-base-part-2-scope-management %})

# Rewrite the WorkerBase class

After fixing the Scope management problem, it's time to rewrite the `WorkerBase` class in a way that
the components are loosely coupling, composable and detachable. The solution turned out to be a very
simple approach. It's the middleware design that is very common in popular Web server frameworks
(ASP.Net Core, Express.js, Koa.js,...).

> In case you don't know what a middleware is, read [ASP.Net Core Middleware](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/middleware/?view=aspnetcore-6.0).

After analyzing the legacy `WorkerBase` class, I found that they could be organized into these
middlewares

- Exception handling middleware
- Logging middleware
- Message queue behaviors middleware
- ...

![Worker middlewares](/files/2021-12-04-refactor-a-legacy-worker-base-part-2/worker-middlewares.png)

# MediatR came to the rescue again!

> Honestly, I love this library!

MediatR [Behaviors](https://github.com/jbogard/MediatR/wiki/Behaviors) bring the Middleware concept
to any MediatR handler class. You can define a Pipeline behavior which is applied for everything or
you can define one applied for specific type only.

Now, let's take a look at how MediatR helps us rewrite and decouple the WorkerBase logic into
detachable components

**Optional**: For simplicity, I will define an interface to represent the request Message body. In
reality, you may not need to do this, but it will be more difficult to parse the data.

```csharp
public interface IMessage
{
    string MessageId { get; set; }
    Guid ClientId { get; set; }
}

public class Worker1
{
    public class Request : IRequest, IMessage // also extend the IMessage interface
    {
        public Guid ClientId { get; set; }
    }

    //...
}
```

I will start with the `ExceptionHandlingBehavior` class

```csharp
public class ExceptionHandlingBehavior<TResponse> : IPipelineBehavior<IMessage, TResponse>
{
    private readonly MessageHandlerContext _messageHandlerContext;

    public ExceptionHandlingBehavior(MessageHandlerContext messageHandlerContext)
    {
        // we can also resolve components from IOC container
        _messageHandlerContext = messageHandlerContext;
    }

    public async Task<TResponse> Handle(IMessage request, CancellationToken cancellationToken,
        RequestHandlerDelegate<TResponse> next)
    {
        try
        {
            return await next();
        }
        catch (HttpException e)
        {
            // your logic here
        }
        catch (DatabaseException e)
        {
            // your logic here
        }
        catch (Exception e)
        {
            // unhandled exceptions will go here
        }

        return default;
    }
}
```

And of course, register it with the IOC Container (Autofac in my case)

```csharp
builder.RegisterGeneric(typeof(ExceptionHandlingBehavior<>)).As(typeof(IPipelineBehavior<,>));
```

Boom! Everything is applied automagically. I don't have to change anything in the `WorkerBase` class
or the `Handler` class.

# Gradually adding new Behavior

Adding a new Behavior to the pipeline is dead easy and doesn't affect any of the existing logic. For
example, I can simply attach a new behavior to track processing time for each message

```csharp
public class MonitoringBehavior<TResponse> : IPipelineBehavior<IMessage, TResponse>
{
    public async Task<TResponse> Handle(IMessage request, CancellationToken cancellationToken,
        RequestHandlerDelegate<TResponse> next)
    {
        var stopWatch = new Stopwatch();
        stopWatch.Start();

        // Activate next handler
        var response = await next();

        stopWatch.Stop();
        TrackProcessingTime(request.MessageId, stopWatch.ElapsedMilliseconds);

        return response;
    }
}

// and then register with Autofac
builder.RegisterGeneric(typeof(MonitoringBehavior<>)).As(typeof(IPipelineBehavior<,>));
```

# Fluent Validation - make it even cleaner

Want a beautiful custom validator? [Fluent Validation](https://fluentvalidation.net/) is here.

First, define a custom Validator and register it with the IOC Container

```csharp
public class Validator : AbstractValidator<Worker1.Request>
{
    public Validator()
    {
        RuleFor(r => r.ClientId).NotEmpty();
    }
}

// register with IOC
public class AutofacModule : Module
{
    protected override void Load(ContainerBuilder builder)
    {
        // register all Validators found in this assembly
        var services = new ServiceCollection();
        services.AddValidatorsFromAssembly(ThisAssembly);
        builder.Populate(services);
    }
}
```

And then, add a Pipeline Behavior to activate the above Validator

```csharp
public class ValidationBehavior<TRequest, TResponse> : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IRequest<TResponse>
{
    private readonly IEnumerable<IValidator<TRequest>> _validators;

    public ValidationBehavior(IEnumerable<IValidator<TRequest>> validators)
    {
        _validators = validators;
    }

    public async Task<TResponse> Handle(TRequest request, CancellationToken cancellationToken,
        RequestHandlerDelegate<TResponse> next)
    {
        var context = new ValidationContext<TRequest>(request);
        var failures = _validators.Select(v => v.Validate(context))
            .Where(r => r != null)
            .SelectMany(r => r.Errors)
            .Where(e => e != null)
            .ToList();

        if (failures.Any())
        {
            throw new ValidationException(failures);
        }

        return await next();
    }
}
```

Until now, I still haven't changed just one line of code in the main `WorkerBase` or `Handler`
class. Everything is detachable and loose coupling!

# A completely new architecture for the WorkerBase

This whole new architecture brings a lot of benefits for the team

- After applying this, people were no longer scared of updating the `WorkerBase`. It actually
  contains very little logic, just the main flow. All the other cross-cutting concerns are split
  into multiple detachable and independent Pipeline Behaviors. We can confidently update one
  component without affecting the other ones.
  - No more files with thousands of lines!
- The library plays nicely with the IOC Container, help us make sure that they only runs within the
  current message scope.
- You have built-in tools for other requirements, including pre-processor, post-processor behavior
  to custom exception handler.
- Depending on the environment, you can attach/detach any behaviors that you want, simply configure
  it in the IOC registration code. For example, you may want to disable the Monitoring behavior in
  Test environment or enable a Caching logic for Production only. Everything is so straightforward.
