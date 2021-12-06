---
layout: post
title: "Refactor part 2"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Part 1:

The refactoring solution I presented in this post is the solution that I wrote in C#. It doesn't
mean you cannot do this in Nodejs. It is just because the team is migrating away from Nodejs to C#.
We are familiar with these tools and they are already available as standard pattern in C#.

# First thing first: An IOC Container

> We learnt this at university. Why the heck did we forget this?

As I mentioned earlier, scope management of the legacy codebase is awful. We actually used a
function to surround the scope of a message and the derived class is actually just a collection of
function, not a scope container. Every time we want to activate a new method, we have to pass all
the parameters downstream.

```javascript
class WorkerBase {
  async start() {
    let message;
    do {
      message = await pullMessages(1);
      const context = this.buildContext(message);

      // this processMessage function wrap the scope of a message
      await this.processMessage(message, context);
    } while (message != null);
  }
}

// Worker service 1
class Worker1 extends WorkerBase {
  myProp = 1;

  async processMessage(message, context) {
    logic1(message, context);
    logic2(message, context);

    myProp++; // this will mutate myProp and affect other message
  }

  logic1(message, context) {}

  logic2(message, context) {}
}
```

> We wrote JS in an OOP way but didn't apply the OOP best practices!

The solution is so simple and is a standard pattern, supported everywhere in C# world. Simply use
any IOC container to create, resolve components and manage the scope. Here is a simple example with
**MediatR** and **Autofac**

The `WorkerBase` class

```csharp
public class WorkerBase<TMessage>
{
    private readonly ILifetimeScope _scope;

    public WorkerBase(ILifetimeScope scope)
    {
        _scope = scope;
    }

    public async Task Start()
    {
        TMessage message;
        do
        {
            var message = await PullMessages<TMessage>();
            await ProcessMessage(message);
        } while (message != null);
    }

    private async Task ProcessMessage(TMessage message)
    {
        await using var innerScope = _scope.BeginLifetimeScope();

        // build context metadata
        var workerContext = innerScope.Resolve<MessageHandlerContext>();
        workerContext.MessageId = message.MessageId;

        // send to handler
        var mediator = innerScope.Resolve<IMediator>();
        await mediator.Send(message.Value);
    }
}
```

and then in the MediatR Handler class, simply resolve the context object from the constructor

```csharp
// The instance of this class is actually a container just for this Message scope
public class Worker1
{
    // The Request is also the model for the Message body
    public class Request : IRequest
    {
        public Guid ClientId { get; set; }
    }

    public class Handler : IRequestHandler<Request>
    {
        private readonly MessageHandlerContext _context;

        // resolve scope item from here
        public Handler(MessageHandlerContext context)
        {
            _context = context;
        }

        public async Task<Unit> Handle(Request request, CancellationToken cancellationToken)
        {
            Logic1(); // no need to pass all data downstream
            Logic2();
        }

        private void Logic1()
        {
            var messageId = _context.MessageId;
            // other logic...
        }

        private void Logic2()
        {
            var messageId = _context.MessageId;
            // other logic...
        }
    }
}
```

Of course, you need to register these items as scoped components instead of singleton ones, for
example

```csharp
// register with Autofac
builder.RegisterType<MessageHandlerContext>().AsSelf().InstancePerLifetimeScope();
```

# Rewrite the WorkerBase class

The solution turned out to be a very simple approach. It's the middleware design that you have seen
in standard web servers.

(I borrowed the middleware pipeline image from [ASP.Net Core](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/middleware/?view=aspnetcore-6.0))

![Asp.Net Core Middlewares](/files/2021-12-04-refactor-a-legacy-worker-base-part-2/asp.net-middlewares.png)

After analyzing the legacy `WorkerBase` class, I found that they could be organized into these
middlewares

- Exception handling middleware
- Logging middleware
- Message queue behaviors middleware
- ...

Benefits

- benefit 1
- benefit 2

# Thank to MediatR

IMessageBody

```csharp
public interface IMessageBody
{
    /// <summary>
    /// The correlationId to correlate the message
    /// </summary>
    string CorrelationId { get; set; }

    /// <summary>
    /// The created timestamp (in milliseconds) of the message
    /// </summary>
    DateTime? CreatedAt { get; set; }

    /// <summary>
    /// The realmId
    /// </summary>
    Guid PublicRealmId { get; set; }

    /// <summary>
    /// The listId
    /// </summary>
    Guid PublicListId { get; set; }

    /// <summary>
    /// The requested by name
    /// </summary>
    string RequestedBy { get; set; }

    /// <summary>
    /// The id of the current message
    /// </summary>
    string MessageId { get; set; }
}
```

Exception

```csharp
public class ExceptionHandlingBehavior<TRequest, TResponse> : IPipelineBehavior<TRequest, TResponse>
    where TRequest : IMessageBody
{
    private readonly ILogTrace _logTrace;
    private readonly ICommonConfig _commonConfig;
    private readonly IWorkerConfig _workerConfig;
    private readonly IMediator _mediator;
    private readonly MessageHandlerContext _messageHandlerContext;

    public ExceptionHandlingBehavior(ILogTrace logTrace, ICommonConfig commonConfig, IWorkerConfig workerConfig,
        IMediator mediator, MessageHandlerContext messageHandlerContext)
    {
        _logTrace = logTrace;
        _commonConfig = commonConfig;
        _workerConfig = workerConfig;
        _mediator = mediator;
        _messageHandlerContext = messageHandlerContext;
    }

    public async Task<TResponse> Handle(TRequest request, CancellationToken cancellationToken,
        RequestHandlerDelegate<TResponse> next)
    {
        try
        {
            return await next();
        }
        catch (OperationSkippedException e)
        {
            _logTrace.Add(LogLevel.Warn, "Message skipped", e.Message);
        }
        catch (Exception e)
        {
            _logTrace.AddError(nameof(ExceptionHandlingBehavior<TRequest, TResponse>), e);
            HandleUnexpectedError(request, e);
        }

        return default;
    }

    private void HandleUnexpectedError(TRequest request, Exception e)
    {
        if (!_commonConfig.SlackEnabled) return;
        if (_workerConfig.OnlyAlertErrorForLastMessageRetry && _messageHandlerContext.WillBeRetried) return;

        // slack data
        var slackChannel = _commonConfig.SlackChannel;
        var hostname = _commonConfig.Hostname;
        var correlationId = request.CorrelationId;
        var realmId = request.PublicRealmId.ToString().ToLower();
        var listId = request.PublicRealmId.ToString().ToLower();
        var messageId = request.MessageId;
        var errorMessage = e.Message.Substring(0, Math.Min(500, e.Message.Length));
        var stackTrace = e.StackTrace.Substring(0, Math.Min(500, e.StackTrace.Length));

        try
        {
            var message = new SlackMessage
            {
                Channel = slackChannel,
                Username = hostname
            };

            message.Texts.Add($"correlationId: *{correlationId}*");
            message.Texts.Add($"messageId: *{messageId}*");

            if (!string.IsNullOrWhiteSpace(realmId))
                message.Texts.Add($"realmId: *{realmId}*");

            if (!string.IsNullOrWhiteSpace(listId))
                message.Texts.Add($"listId: *{listId}*");

            message.Texts.Add($"```{errorMessage}```");

            message.Attachments = new List<SlackAttachment>
            {
                new SlackAttachment
                {
                    Color = SlackColor.Danger,
                    Title = "Stack Trace",
                    Text = $"```{stackTrace}```"
                }
            };

            _mediator.Send(new SendMessage.Request {Message = message, HasError = true});
        }
        catch (Exception exception)
        {
            _logTrace.AddError(nameof(HandleUnexpectedError), exception);
        }
    }
}
```
