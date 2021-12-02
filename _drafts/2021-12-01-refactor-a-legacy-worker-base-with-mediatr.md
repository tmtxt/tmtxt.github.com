---
layout: post
title: "Refactor a legacy Worker Base - Part 1 - The Problems"
description: ""
categories: []
tags: []
thumbnail:
---

# How it all began

First, we started with this inheritance model, the way that most people will think of when they
starting implementing a Worker base module. We define a base class for our Worker Base. This
contains all the logic to pull the message from the corresponding queue, do some validation and
active the main handle function. The derived class only needs to implement one function to handle
the main business logic of that worker.

```javascript
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

class Worker1 extends WorkerBase {
  constructor() { super({ queueName: 'Worker1' }); }

  async processMessage(message) {
    // implement worker1 logic here
  }
}

class Worker2 extends WorkerBase {
  constructor() { super({ queueName: 'Worker2' }); }

  async processMessage(message) {
    // implement worker2 logic here
  }
}
```

This is simple and does the job well. However,...

# And then we wanted to add...

As the product growing, we wanted to add more logic to our `WorkerBase`
class. The first thing, of course, was some logging and monitoring code to support basic
troubleshooting. We simply modified the `start()` function of the `WorkerBase` class and added these
common things

```javascript
// logging some message information
logger.info(message.id);
logger.info(message.createdAt);

// track some metrics
await trackProcessingTime(this.queueName, message.id, elapsedTime);
await trackSuccessRate(this.queueName, message.id);
```

After that, we also wanted to make use of some characteristics of a Message Queue. We decided to
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
handler to throw error.

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

You can imagine yourself how much extra logic that we have added after 7-8 years of development.

# How about managing Scope?

Often in worker that processes messages from a Message Queue, you will want to separate the scope
for each message so one process doesn't overwrite the data created from another message. Also, in
each scope, you may want share some context data. With the above design, each call to the
`processMessage` function creates a child and we have no way but passing a `context` object
downstream to all the callee functions that need to consume the `context` object.

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

Let take a look at the `WorkerBase` class after we added all the above requirements.

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
