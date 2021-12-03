---
layout: post
title: "Refactor a legacy Worker Base - Part 1 - The long lasting pain"
description: ""
categories: [misc]
tags: []
thumbnail:
---

> Let's talk about Microservices again!

In order to manage a Microservices system efficiently, people usually enforce all the
microservices to
follow some common patterns. This helps you standardize the monitoring process and add a new
microservice more easily. In the Microservices project that I'm currently working on (at Agency
Revolution), we also have to
implement a base class for the each different type of microservice. The base
class contains the logic to write log in the correct format, to handle some common errors
correctly or to alert the developers if something go wrong, etc.

Basically, there are 2 types of Microservices in
our system: **Synchronous** and **Asynchronous**. I will focus mostly on one type of Async worker in
this post: **The Message Queue workers**.
The base class was initially built in Nodejs. After several years of development, we started to face
many problems with the design. And now, I'm going to show you how I identified the drawbacks and
improved it with a better version in C#.

> Why C#? I may explain in another post. But right now, you
> can take a look at this
> [post]({% post_url 2021-08-09-clean-architecture-with-csharp-net-core-and-mediatr %})
> first.

# How it all began

First, we started with this Inheritance model, the way that most people will think of when they
start implementing a Worker base module. We defined a super class that all the workers in the system
can derive from. It
contains the logic to pull messages from the corresponding queue and
activate the main handler function.

```javascript
// This is Javascript code
// The base class
class WorkerBase {
  constructor(config) { this.queueName = config.queueName; }

  async start() {
    let message;
    do {
      message = await pullMessages(1); // pull 1 message at a time
      await this.processMessage(message);
    } while (message != null);
  }

  // implement this in the derived class
  async processMessage(message) { throw new Error('Not implemented'); }
}

// Worker service 1
class Worker1 extends WorkerBase {
  constructor() { super({ queueName: 'Worker1' }); }

  async processMessage(message) {
    // implement worker1 logic here
  }
}

// Worker service 1
class Worker2 extends WorkerBase {
  constructor() { super({ queueName: 'Worker2' }); }

  async processMessage(message) {
    // implement worker2 logic here
  }
}

// to activate a worker
const worker = new Worker1();
await worker.start();
```

<!-- more -->

This is simple and does the job well.

# And then we wanted to add...

As the product growing, we wanted to add more logic to our `WorkerBase`
class. The first thing, of course, was some logging and monitoring code to support basic
troubleshooting. We simply modified the `start()` function of the `WorkerBase` class and added
those common things

```javascript
// logging some message information
logger.info(message.id);
logger.info(message.createdAt);

// track some metrics
await trackProcessingTime(this.queueName, message.id, elapsedTime);
await trackSuccessRate(this.queueName, message.id);
```

After that, we also wanted to make use of the common Message queue methods. We decided to
wrap most of the main logic of the `start()` function in a `try`/`catch` block

```javascript
try {
  await this.processMessage(message);

  // other logic mentioned before...

  await acknowledgeMessage(message);
} catch (e) {
  await markMessageAsFailed(message);
}
```

It started to get complicated when we introduced some custom error handling mechanisms.
The above `catch` block was extended, which made it more error-prone and might cause the error
handler to throw error, too.

```javascript
try {
  // other logic...
} catch (e) {
  if (e instanceof HttpError) {
    await trackMonitoringMetric(e);
  }

  if (e instanceof DatabaseError) {
    await trackDatabaseMetric(e);
  }

  // other logic...
}
```

How about managing Scope? Often in a worker that processes messages from a Message Queue, you
will want to separate the scope for each message so one message doesn't overwrite the data created
from another one. Also, in each scope, you may want share some context data. With the above
design, each call to the `processMessage` function creates a child context and we have no way but
passing a `context` object downstream to all the callees that consume the `context` object.

```javascript
messages = await pullMessages(10); // assume now you want to process 10 messages at a time
const processInScope = async (message) => {
  var contextData = {
    processId,
    trackingDisabled,
    // other props
  };

  // pass this downstream
  await this.processMessage(message, contextData);

  // and maybe downstream to other functions
  await trackProcessingTime(this.queueName, message.id, elapsedTime, contextData);
};
await Promise.all(messages.map(processInScope));
```

# A straightforward but not scalable solution

Let take a look at the `WorkerBase` class after we have added all the above requirements.

```javascript
class WorkerBase {
  async start() {
    let messages;
    do {
      messages = await pullMessages(10); // pull 10 messages at a time

      try {
        const processInScope = async message => {
          // logging some message information
          logger.info(message.id);
          logger.info(message.createdAt);

          const contextData = { processId, trackingDisabled };
          await this.processMessage(message, contextData);

          // track some metrics
          await trackProcessingTime(this.queueName, message.id, elapsedTime, contextData);
          await trackSuccessRate(this.queueName, message.id, contextData);

          await acknowledgeMessage(message);
        };
        await Promise.all(messages.map(processInScope));
      } catch (e) {
        if (e instanceof HttpError) {
          await trackMonitoringMetric(e);
        }

        if (e instanceof DatabaseError) {
          await trackDatabaseMetric(e);
        }

        await markMessageAsFailed(message);
      }
    } while (messages.length !== 0);
  }

  // others...
}
```

As you can see, there are various issues here.

- After nearly 10 years of development, more and more features were added to the system, we
  finally ended up with a `WorkerBase` class that is more than 2500 lines long. Everybody is scared
  of touching it.
- It is hard to add a new feature to the `WorkerBase` class. We have to scan the main
  function (which is very long), find the correct place, apply your logic and pray that it doesn't
  break the other ones.
- It is hard to test since all the logic are centralized into one place. The above class contains
  not only the primary logic but also the other cross-cutting concerns. Every time we want to test
  just one line of code, we will have to run through all the above stuff.
- It is hard to enable/disable one specific feature. In order to do that, we have to update our code
  with a new `if`/`else` clause (which also makes the `start` function become longer).
- Scope management is awful. We actually don't have anything for this job beside the function
  scope. A worker instance is actually just a collection of functions, not a scope container. As a
  result, we have to pass all the context data to all the subsequent functions.

The most trivial solution is, of course, to separate them into smaller functions to make it shorter.

> IMO, it's the solution of a fresher.

A better way would be to build them as **composable components**.

# To be continued...
