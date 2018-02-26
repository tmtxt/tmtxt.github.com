---
layout: post
title: "Basic Logging & Debugging in Microservices 1"
description: ""
categories: []
tags: []
thumbnail:
---

One of the biggest difficulties when working with Microservices (or with other Distributed systems)
is to debug if any problems occur. It is because the business logic is divided into several small places.
The code bug in one service can result in a cascading series of issues in many related services.
Tracing which service is the root cause of the issue is always a challenging mission. By
implementing a good Logging solution, you can reduce the time it takes to discover the bug. It also
helps you feel more confident about what happened in your code as well as makes the problem easier
to reason about.

# Let's get your feet wet!

So you decided it's time to build a logging solution for your Microservices system, here are some
steps that you probably need to do in order to build that.

- First, design and implement your logging module so that it works well in one microservice.
- Apply it to all the services in the system.
- Implement a method to link all the correlated logs in different services.
- Set up centralised logging server for processing and querying the log data.
- Define which data you need to put into the log entries for better investigation.

# Design your Logging library

Before starting with a full Logging solution for the whole large application, it is important that
you get your smallest building block to work properly. You will first need to build a logging
solution that can work well in one service, and then apply to all other services. You have to define
a logging standard that all the other services will follow so that you can store all the
log entries into another logging backend storage for later investigation.

The simplest logging way is to write the log immediately whenever you want. For example, when you
receive one API request, when the HTTP request is done processing or when the server finishes
update one record in the database. However, you will soon end up with a bunch of messy log entries
because the web server usually processes multiple requests at the same time and you don't know which
ones have the correlation with the others. This is quite common in the concurrent and parallel world
where the system can handle different tasks at once. You need to design a logging backend that can
associate all the related log entries into one.

<!-- more -->

Your logging solution should support different logging level, from `verbose` (or `debug`), `info` to
`warning` and `error`. It also needs to be dynamically configurable through environment variable so
that you can easily increase the detailed level for important services or reduce the logging size of
less critical services to save storage. This can be achieved quickly with all the common logging
libraries in all programming languages. These logging libraries can also include extra metadata into
the log entries so that you can use other database to persist and index those information to quickly
retrieve the correct log entry.

Another thing you need to consider is to output the log data in different formats depending on the
environment that the app is running on. For example, you may want to print the log messages in a
human-readable format when working in local development environment. However, on production, the log
messages should be output as JSON format to for smaller log size and easier to transfer to another
storage for indexing and displaying.

Here are the summarised characteristics that your logging module should provide

- Follow one defined standard
- Can associate related log entries into one entry and flush them all at once
- Need to support different logging level and be configurable through environment variables
- Be able to keep additional information for easier querying
- Support different logging methods depending on the environment (local development,
  staging, production,...)

Of course, you are not limited to only the above criteria. In your implementation, you can also add
some extra feature like processing time tracking (which you will see in the example code below),
auto formatting the messages,...

> In my opinion, the above logging principals are applied not only for Microservices system but also
> other Monolith and Distributed systems. The only difference in Microservices is how to link the
> related log entries in different services, which will be discussed later.

# Implement your Logging library

Implementing the above Logging module turns out to be a simple task. All the logging libraries out
there on the Internet already support most of the requirements above. Basically, you will need to
implement a wrapper class with an array variable to keep track of all the related log entries and
write them all at once. There will be 2 exposed functions, one for pushing more data to the log
array and the other one for writing all the logs at the same time. Here is a simple example in
NodeJS implemented using `winston` but you can use any logging library that you want.

- The wrapper class structure will look like this

```js
class MyLogger {
  // an array prop to keep track of all related logging data
  messages = [];
  // meta data
  metaData = {};
  constructor(metaData) {
    this.metaData = metaData || {};
  }

  /**
   * Append one new log entry into the mesage array
   * @param {string} logLevel  error|warning|info|verbose
   * @param {string} logTitle
   * @param {any}    message
   */
  push(logLevel, logTitle, message) {
    // implementation goes here
    ...
  }

  /**
   * Write all the log data at once in an single log entry
   */
  write() {
    // implementation goes here
    ...
  }
};
```

- The `push` method only stores the log data into the `messages` array. For example

```js
class MyLogger {
  // other props
  ...

  push(logLevel, logTitle, message) {
    if (!message) {
      message = '';
    }

    if (_.isObject(message)) {
      message = JSON.stringify(message, null, 2);
    }

    this.messages.push({ logLevel, logTitle, message });
  }

  // other messages
  ...
};
```

- The `write` method is responsible for combining and writing out the all the log data into one
  single log entry

```js
class MyLogger {
  // other props
  ...

  write() {
    // combine and format the messages
    const messageStr = this.formatMessages();
    // detect the log level for combined log entry
    const logLevel = this.detectLogLevel();
    // metaData
    const metaData = this.metaData;

    // actually write the log entry
    logger[logLevel](messageStr, metaData);
  }

  // other messages
  ...
};
```

Here is the full implementation of the `MyLogger` class above:
[MyLogger full implementation](https://github.com/tmtxt/tmtxt.github.com/blob/master/files/2018-02-22-basic-logging-monitoring-in-microservices-1/my-logger.js)

# Apply it into your application

The idea is that you will keep one single Logger instance for each HTTP request, push all the
related log entries into that instance and write everything at the end of the request. For example

```js
// POST /api/login
function* handleLogin() {
  const logger = new MyLogger({
    url: this.request.url,
    apiName: 'handleLogin',
    contentType: this.request.contentType
  });

  this.body = 'Incorrect username or password';
  this.status = 401;

  // parse request body data;
  const body = this.requestBody;
  const username = body.username;
  const password = body.password;

  // check if the user exist in the database
  const user = yield User.getByUsername(username);
  if (!user) {
    logger.push('warn', 'handleLogin', 'User does not exist');
    return logger.write();
  }
  logger.push('info', 'handleLogin', 'User exists');

  // validate whether the user is still active
  if (!user.isActive) {
    logger.push('warn', 'handleLogin', 'User not active');
    return logger.write();
  }
  logger.push('info', 'handleLogin', 'User is still active');

  // validate password
  if (hashPassword(password) !== user.password) {
    logger.push('warn', 'handleLogin', 'Password not match');
    return logger.write();
  }
  logger.push('info', 'handleLogin', 'Password matched');

  // create auth token
  const authToken = generateAuthToken(user);
  logger.push('info', 'handleLogin', 'Auth Token generated');

  // response to user
  this.body = { authToken };
  this.status = 200;

  // write everything at once
  logger.write();
}
```

Of course, the above example is a bit verbose and repetitive. In the next post, I will come back to
that.

# Compare with the default Logger

The output you get will be something look like this

- Fail login

![Fail Log](/files/2018-02-22-basic-logging-monitoring-in-microservices-1/fail-log.png)

- Success login

![Success Log](/files/2018-02-22-basic-logging-monitoring-in-microservices-1/success-log.png)

Pretty nice, huh! This is especially useful when there are multiple requests being processed at the
same time. Compare these 2 logging solutions and you will see

- Default `Logger`

![Default Log](/files/2018-02-22-basic-logging-monitoring-in-microservices-1/traditional.png)

- Custom `MyLogger`

![Custom MyLogger](/files/2018-02-22-basic-logging-monitoring-in-microservices-1/custom.png)

The custom `MyLogger` provides a better view for tracing and debugging what happened in each API
call. It also groups all the related logging entries into one single log entry.

# Summary

That is the very first part in building a full Logging & Tracing system in Microservices. You don't
need to follow the exact steps or implementation above. The basic idea is to implement some
mechanism to better group the related log entries in a the concurrent world and provide a better
view for tracing and debugging. It will help you build a stable ground to grow your system to bigger
size. In the parts, I will talk about how we should integrate it into the application, how we should
link the log data between services as well as how we should improve that logging system to respond
to the bugs/incidents as quick as possible.

# To Be Continued...
