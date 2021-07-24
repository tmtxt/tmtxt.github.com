---
layout: post
title: "Clean architecture with C#/.Net Core and MediatR - Part 2"
description: ""
categories: []
tags: []
thumbnail:
---

---

Nodejs has been a pain point in our code base for year. We started with Nodejs several years ago. It
is one of the best choice for starting a product because of the ease in development.
... continue here

# Clean Architecture revisit

Let's take a look at this very famous Clean Architecture demonstration image

![Clean Architecture](/files/2021-07-13-clean-architecture/clean-architecture.jpg)

To make it simpler, I will re-draw it a bit.

Here is how the data flow through the application

![Data Flow](/files/2021-07-13-clean-architecture/data-flow.png)

Here is how the Projects in C# look like. The outer project references the inner one

![Reference](/files/2021-07-13-clean-architecture/reference.png)

The arrows that I drew are also a bit different from the original Clean architecture flow but it's
actually the same. The Data flow is

- The core Business logic resides in **Business Layer**. They are pure C#, independent from the
framework, database or any other external services. It doesn't care if one request comes from an
Http API, from a Timer Worker or a Script.
- The framework outside needs to wrap the request, transform it into the same input format that the
**Business Layer** expects.
- The **Business Layer** also contains a list of interfaces to interact with all the external
dependencies (anything outside of the C# applications, from the storage layer to external services).
The implementations will be delegated to the outer circle to make the **Business Layer** become
loose-coupling with all those external dependencies.

Fact: **Naming** is hard. I don't know if the above names are correct. They are not even the ones
that I used in my code :joy:. Well, names are just names, as long as we understand what they do,
that's enough.

Fact: You may have more than 3 layers. They are just for demonstration purpose

# Business Layer


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
