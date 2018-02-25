---
layout: post
title: "Basic Logging & Debugging in Microservices 1"
description: ""
categories: []
tags: []
thumbnail:
---

One of the biggest difficulties when working with Microservices (or with other Distributed systems)
is to debug if any problems occur. It is because the business is divided into several small places.
The code bug in one service can result in a cascading series of issues in many related services.
Tracing which service is the root cause of the issue is always a challenging mission. By
implementing a good Logging solution, you can reduce the time it takes to discover the bug. It also
helps you feel more confident about what happened in your code as well as makes the problem easier
to reason about.

# So let's get your feet wet!

So you decided it's time to build a logging solution for your Microservices system, here are some
steps that you probably need to do in order to build a logging system for your Microservices system.

- First, design and implement your logging module so that it works properly in one microservice.
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

Your logging solution should support different logging level, from `verbose` (or `debug`), `info` to
`warning` and `error`. It also needs to be dynamically configurable through environment variable so
that you can easily increase the detailed level for important services or reduce the logging size of
less critical services to save storage. This can be achieved quickly with all the common logging
libraries in all programming languages. These logging libraries can also include extra metadata into
the log entries so that you can use other database to persist and index those information to quickly
retrieve the correct log entry.

Another thing you need to consider is to output the log data in different formats depending on the
environment that the app is running on. For example, you may want to print the log messages in a
human-readable format when working in local development. However, on production, the log messages
should be output as JSON format to for smaller log size and easier to transfer to another storage
for indexing and displaying.

Here are the summarised characteristics that your logging module should provide

- Follow one defined standard
- Can associate related log entries into one entry and flush them all at once
- Need to support different logging level and be configurable through environment variables
- Be able to keep additional information for easier querying
- Support different logging methods depending on the environment (local development,
  staging, production,...)

> In my opinion, the above logging principals are applied not only for Microservices system but also
> other Monolith and Distributed systems. The only differences in Microservices is how to link the
> related log entries in different services, which will be discussed later.
