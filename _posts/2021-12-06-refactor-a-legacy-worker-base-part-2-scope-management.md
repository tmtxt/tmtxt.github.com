---
layout: post
title: "Refactor a legacy Worker Base - Part 2 - Scope Management"
description: "Let's fix the Scope Management first"
categories: [.net,javascript]
tags: []
thumbnail:
---

> Part 1: [Refactor a legacy Worker Base - Part 1 - The long lasting pain]({% post_url 2021-12-03-refactor-a-legacy-worker-base-part-1-the-long-lasting-pain %})

The refactoring solution I presented in this post is the solution that I wrote in C#. It doesn't
mean you cannot do this in Nodejs. It is just because the team is migrating away from Nodejs to C#.
We are familiar with these tools and they are already available as standard pattern in C#.

# First thing first: An IOC Container

> We learnt this at university. Why the heck did we forget this?
> Is it because the program language allows us to make this mistake so easily or is it because of
> the community that encourages the bad behaviors everywhere?

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

<!-- more -->

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
        // use the IOC container to wrap an inner scope around message handler process
        await using var innerScope = _scope.BeginLifetimeScope();

        // build context metadata
        var workerContext = innerScope.Resolve<MessageHandlerContext>();
        workerContext.MessageId = message.MessageId;

        // send to MediatR handler
        var mediator = innerScope.Resolve<IMediator>();
        await mediator.Send(message.Value);
    }
}
```

and then in our **MediatR** `Handler` class, simply resolve the context data in the constructor

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

# The benefits?

- Scope management becomes dead easy, even for nested scopes.
- The components are loosely coupling, that means you can easily switch the implementation in
  different environments (Prod, Staging, Test)
- Since the components are managed by the IOC Container, which is independent from the business
  logic, we can easily even switch the IOC Container logic to embed the Handler class logic into a
  different runtime (Http Server, Timer worker, One-of script, Lambda function,...)

# To be continued...
