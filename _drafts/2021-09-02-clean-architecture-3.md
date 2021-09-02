---
layout: post
title: "Clean architecture with C#/.Net Core and MediatR - Part 3"
description: ""
categories: []
tags: []
thumbnail:
---


# MediatR FTW

The above design is an interface design. Everything is connected via interfaces and a hidden
implementation. The requests coming from the framework to the Business handler via Business
interfaces. The requests from the Business circle to the database or other services are also done
using interfaces (so that we can delegate the implementation to other layer).

**Mediator** pattern and **MediatR** library are just another way to define the interface and the
handler (via the `TRequest`/`TResponse` type). In fact, you can simply define your own interface and
attach the corresponding implementation through the IOC container. However, the main reason I use
**MediatR** is because of its excellent dynamic pipeline behavior, which helps me separate a lot of
cross-cutting concerns out of the main handler, makes everything cleaner and produces a concise,
testable handler class.

A very simple interface and implementation in **MediatR** looks like this

```csharp
public class GetUser
{
    public class Request : IRequest<Response>
    {
        public int UserId { get; set; }
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
            var user = await SomeFunction(cancellationToken);

            return new Response
            {
                User = user
            }
        }
    }
}
```


## 1. Terms

- **Business layer** is where your application logic resides. It's pure C#, independent from the
  framework, database or any external dependencies.
- **Controller layer** is where we map out the underlying implementations of all the contracts (interfaces) we
  defined in **Business layer**.
- **Infrastructure** is actually related to the deployment and the operations of the other external
  cross-cutting concerns







Data flow: ....

Fact: naming is hard



# Business Layer

# Controller Layer

# Infrastructure Layer
