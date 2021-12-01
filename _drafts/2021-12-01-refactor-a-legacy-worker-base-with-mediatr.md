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
      message = await pullMessage();
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

This is straightforward and does the job well. However,...

# And then we want to...

## Add some logging logic

In this case, simply add more logic to the `start()` function of the `WorkerBase` class

```javascript
async start() {
  let message;
  do {
    message = await pullMessage();
    await this.processMessage(message);

    // logging
    logger.info(message.id);
    logger.info(message.createdAt);
    // ...other logging logic
  } while (message != null);
}
```

Still ok

## Integrate with our monitoring service

Keep adding to monitor

```javascript
async start() {
  let message;
  do {
    // other logic...

    // monitoring
    await trackProcessingTime(this.queueName, message.id, elapsedTime);
    // ...other tracking logic
  } while (message != null);
}
```

## Introduce some basic logic of Message Queue

```javascript
async start() {
  let message;
  do {
    // other logic...

    try {
      await this.processMessage(message);

      // other logic
      await acknowledgeMessage(message);
    } catch (e) {
      await markMessageAsFailed(message);
    }

    // ...other logging logic
  } while (message != null);
}
```

## And then implement some custom error handlers

```javascript
async start() {
  let message;
  do {
    try {
      // other logic...
    } catch (e) {
      if (e instanceof HttpError) {
        await trackMonitoringMetric(e);
      }

      if (e instanceof DatabaseError) {
        await trackDatabaseMetric(e);
      }

      // other error handlers...

      await markMessageAsFailed(message);
    }

    // ...other logging logic
  } while (message != null);
}
```

## How about passing Process' context data?

```javascript
async start() {
  // other logic...

  var contextData = {
    processId,
    trackingDisabled,
    // other props
  };
  await processMessage(message, contextData);

  // monitoring
  await trackProcessingTime(this.queueName, message.id, elapsedTime, contextData);

  // other logic...
}
```

# A simple but not scalable solution
