---
layout: post
title: "Scaling the System at AR - Part 2 - Message Queues"
description: ""
categories: [misc]
tags: []
thumbnail: /files/ar-logo-1.png
---

> It has been one and a half year since my first post about this topic :(

Continue from my previous post
[Scaling the System at AR - Part 1 - Data Pre-Computation]({%post_url 2018-08-08-scaling-the-system-at-ar-part-1%}),
this time I'm going to talk about one of the most important component of the system at AR:
**The Message Queues**.

A message queue

# A Web hook API backed by a Message Queue

> This method doesn't apply for other types of public API, mostly just web hook

In our system, there are several public API endpoints we used to receive the updated data
from the other system, including web hook API to receive updated email events from Mailgun,
integration API to receive updated customer data from third party partners that we integrate with.
There are 2 factors that we consider the most important for a public web hook API

- It should respond fast enough so we can handle a large amount of data update. Our system sends
  a lot of emails every day. We also integrate with a lot of data from multiple insurance agencies
  across the US.
- Once the request has been confirmed, it should never lose the data until the data is successfully
  processed. No matter the code has bug or not, eventually, it should always process the data
  thoroughly and display to the user.

Those criteria match exactly with what a Message queue can offer. Most of our public API endpoints
are backed by a Message queue, which defers and ensures the success of data processing.

![API](/files/2020-03-15-message-queue/api.png)

Basically, the API server is designed as simple as possible. It performs only some simple
validations on the input data, make sure they follow the correct schema and then publish a message
with the request as the body to a queue. The API then responds back to the client immediately, with
a message promising that the data will be processed at some point in the future.
