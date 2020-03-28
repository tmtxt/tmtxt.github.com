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

A message queue is an asynchronous inter-process communication pattern. It can be the communication
between processes in the same applications or across different applications.

# A Webhook API backed by a Message Queue

> This method doesn't apply for other types of public API, mostly just webhook API

In our system, there are several public API endpoints we use to receive the updated data
from the other systems, including webhook API to receive updated email events from Mailgun,
integration API to receive changed customer data from third party partners that we integrate with.
There are 2 factors that we consider the most important for a public webhook API

- It should respond fast enough so we can handle a large amount of data update. Our system sends
  a lot of emails every day. We also integrate with a lot of data from multiple insurance agencies
  across the US.
- Once the request has been confirmed, it should never lose the data until the data is successfully
  processed. No matter the code has bug or not, **eventually**, it should always process the data
  thoroughly and display to the user.

Those criteria match exactly with what a Message queue can offer. Most of our public API endpoints
are backed by a Message queue, which defers and ensures the success of data processing.

![API](/files/2020-03-15-message-queue/api.png)

Basically, the API server is designed as simple as possible. It performs only some simple
validations on the input data, make sure they follow the correct schema and then publish a message
with the request as the body to a queue. The API then responds back to the client immediately, with
a message promising that the data will be processed at some point in the future. In the background,
there is a worker that subscribes to this queue to consume the messages. It is
responsible for processing all the complex logic related to the request data, for example, wrapping
data from multiple data sources, performs some CPU-intensive operations to convert the data to our
schema or handle the activities logging.

There are some benefits of moving all the complex logic from the API server to the background
worker.
- Your public API is simpler. That means you can serve more requests and reduce the downtime
  risk of your public API.
- The message queue ensures that all the data will eventually be processed successfully. Even if
  there is a bug in the code, we won't lose the data. The failed messages will go back to the queue
  (depending on the retry policy) or go to another Error queue, waiting for us to fix the bug and
  then retry the messages

# An Event Updater system backed by a Message Queue

This is the reverse of the above one. In previous section, a Message Queue is used to handled
updated data from other systems to our system. This section is about using a Message Queue to update
event back to the other systems.

The main product of AR (or FMGSuite now) is an Automation marketing platform. We do not provide a
CRM/AMS solution to our clients. Instead, we try to integrate with many popular products out
there. There are many cases that we want to write the data back to the those systems, for instance,
to write back the feedback of the customers of our clients or to insert a Prospect customer when a
user fill in a web form. The requirements are also quite similar

- The application should be fast enough so the user don't have to wait forever waiting for a form to
  be submitted.
- The data must be **eventually** written to the other systems.

Here is a sample design of a Form submitting flow

![API](/files/2020-03-15-message-queue/form.png)

Similarly, the user-facing API on the web app doesn't do any complex logic. It just tries to
validate and capture the user-input data as fast as possible and then defers the task to another
background worker. Of course, this design brings some benefits compare to putting all the logic in
the web API

- Your web app will respond faster, the users will feel more comfortable, the app looks more
  responsive.
- You will never lose the customer data. In case the 3rd-party system is down or any other network
  issue, the messages are still kept in the queue, retries multiple times until success or moved to
  another Error queue waiting to be re-queued at one point in the future.

# Isolate and Allocate resources with Message queues

One of the difference of the system at AR is that most of the tasks are background tasks and backed
by several message queues. There are several reasons for us to choose this design

- We want to keep the user-facing API and databases simple fast. This way, the all the API respond
  very fast and the app performs more smoothly. That brings a good impression to our users.
