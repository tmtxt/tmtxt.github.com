---
layout: post
title: "Basic Logging & Debugging in Microservices - Part 2"
description: ""
categories: [misc]
tags: [misc]
thumbnail: 
---

> First part here [Basic Logging & Debugging in Microservices - Part 1]({% post_url 2018-02-26-basic-logging-monitoring-in-microservices-1 %})

In previous post, I have talked about the advantages of building a custom Logger module that can group all the related log data into one single log entry. In this post, I will continue discussing about some basic ideas to integrate it into the Microservices architecture and organise all the log data for better investigation.

# Integrate custom MyLogger into your application

Integrating the custom MyLogger module into the application is a quite straightforward task. Instead of manually initialising and flushing the logs, you will need to implement a wrapper or higher order function to do that automatically. For example, if you are using Koa.js to for your http service, simply wrap the requests inside a logger middleware like this

```js
const MyLogger = require('./my-logger.js')

// initialise koa app
// ...

function* myLoggerMdw(next) {
// init logger and assign to the context
  const metadata = {
    appName: 'your-service-name',
    routeName: this.request.routeName
  };
  const logger = new MyLogger(metadata);
  this.logger = logger;
  
  // wrap logger around your request
  try {
    logger.push('info', 'Start request', this.request.headers);
    yield next;
    logger.push('info', 'Request success', this.status);
    logger.write();
  } catch(e) {
    if (e.status < 500) {
      logger.push('warn', 'Handled error', e.message);
    } else {
      logger.push('error', 'Unhandled error', e.message);
    }
    logger.write();

    throw e;
  }
}

// custom middleware for MyLogger
app.use(myLoggerMdw)
```

In your request handler function, just get the `logger` instance directly from the request context and use it normally.

```js
// POST /api/login
function* handleLogin() {
  this.body = 'Incorrect username or password';
  this.status = 401;

  // parse request body data;
  const body = this.requestBody;
  const username = body.username;
  const password = body.password;

  // check if the user exist in the database
  const user = yield User.getByUsername(username);
  if (!user) {
    return this.logger.push('warn', 'handleLogin', 'User does not exist');
  }

  // validate whether the user is still active
  if (!user.isActive) {
    return this.logger.push('warn', 'handleLogin', ‘User not active');
  }

  // validate password
  if (hashPassword(password) !== user.password) {
    return this.logger.push('warn', 'handleLogin', 'Password not match');
  }

  // create auth token
  const authToken = generateAuthToken(user);
  this.logger.push('info', 'handleLogin', 'Auth Token generated');

  // response to user
  this.body = { authToken };
  this.status = 200;
}
```

This implementation is much better than the implementation in the first part. You don’t need to care about whether to call the `write` function everytime the request finishes processing.

# Track the request life-cycle in Microservices

The above implementation just helps you to group all the log data of 1 request in 1 microservice into 1 single log entry. However, because of the nature of Microservices, each request can go through several services. The request life-cycle is spread across multiple places. You need another mechanism to correlate all those log entries produced by several services so that you can trace what happened inside that one request.

The simplest idea is to assign each request a unique id when first processing that. Your application need to maintain that unique id through out the request life cycle. For http services, you can keep that information the request headers. For services that operate based on a message queue (like Google PubSub, Kafka), you can maintain that id inside the message metadata.

Back to the above `myLoggerMdw` above, here is the modified code for tracking that unique id. The basic idea is that the http server will try to compute the `correlationId` from the request headers if exist, otherwise, it will generate a new one and pass it to all other services through the subsequent request headers.

```js
function* myLoggerMdw(next) {
  // init logger and assign to the context
  const correlationId = this.request.headers['my-correlation-id'] || uuid.v4();
  
  const metadata = {
    appName: 'your-service-name',
    correlationId
  };
  const logger = new MyLogger(metadata);
  this.logger = logger;
  
  // wrap logger around your request
  ...
}

// custom middleware for MyLogger
app.use(myLoggerMdw)
```

![with correlationId](/files/2018-02-22-basic-logging-monitoring-in-microservices-1/success-log.png)

# Next

With this post, I have demonstrated some basic ideas about correlating all log data of one request in Microservices architecture. In the next post, I will talk about how to organise all those log entries into one single log server for better querying and visualizing.