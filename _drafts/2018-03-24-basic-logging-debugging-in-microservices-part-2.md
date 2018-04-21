---
layout: post
title: "Basic Logging & Debugging in Microservices - Part 2"
description: ""
categories: []
tags: [misc]
thumbnail: 
---

In previous post, I talked about the advantages of building a custom Logger service that can all the related log data into one single log entry. In this post, I will continue discussing about some basic ideas to integrate it into the Microservices architecture and organise all the log data for better investigation.

# Integrate custom MyLogger into your application

Integrating the custom MyLogger module into the application is quite a straightforward task. Instead of manually initialising and flushing the logs, you will need to implement a wrapper or higher order function to do that automatically. For example, if you are using Koa.js to implement your http service, simply wrap the requests inside a logger middleware like this

```js
const MyLogger = require(‘./my-logger.js’)

// initialise koa app
// ...

function* myLoggerMdw(next) {
// init logger and assign to the context
  const metadata = {
    appName: ‘your-service-name’,
  };
  const logger = new MyLogger(metadata);
  this.logger = logger;
  
  // wrap logger around your request
  try {
    logger.push(‘info’, ‘Start request’, this.request.headers);
    yield next;
    logger.push(‘info’, ‘Request success’, this.status);
    logger.write();
  } catch(e) {
    if (e.status < 500) {
      logger.push(‘warn’, ‘Handled error’, e.message);
    } else {
      logger.push(‘error’, ‘Unhandled error’, e.message);
    }
    logger.write();

    throw e;
  }
}

// custom middleware for MyLogger
app.use(myLoggerMdw)
```