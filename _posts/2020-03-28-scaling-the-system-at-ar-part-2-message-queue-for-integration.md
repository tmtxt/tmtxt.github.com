---
layout: post
title: "Scaling the System at AR - Part 2 - Message Queue for Integration"
description: "Continue from my previous post, this time I'm going to talk about one of the most important component of the AR system: The Message Queue"
categories: [misc]
tags: []
thumbnail: /files/ar-logo-1.png
---

> It has been one and a half year since my first post about this topic :(

Continue from my previous post
[Scaling the System at AR - Part 1 - Data Pre-Computation]({%post_url 2018-08-08-scaling-the-system-at-ar-part-1-data-pre-computation%}),
this time I'm going to talk about one of the most important component of the AR system:
**The Message Queue**.

Message Queue is an asynchronous inter-service communication pattern. It is a temporary place to
store the data, waiting for the message receiver to process. It encourages decoupling
of logic and components in the system, provides a lightweight and unified protocol for communication
between different services (written in different languages) and is perfectly suitable for
Microservice design. A good message queue should satisfy these criteria

- It must be fast and capable of handling a large amount of messages coming in at the same time.
- It have to ensure the success of message processing. A message must be processed and retried until
  success. Otherwise, an Error queue (Dead Letter queue) should be provided to store the failed
  messages for later processing.
- It is required that each message is processed by one and only one consumer at the same time.
- The message queue should be independent from any languages and allow various applications
  written in different languages to send and receive messages without any problem.

Because the Message Queue is so important to us and there is a limit in number of developers, we
decided to switch to third-party services after several months doing both dev and ops work with Kafka.
Both **Google Pub/Sub** and **AWS SQS** offer the service in a relatively cheap price and you can
choose either of them, depending on the Cloud platform that you are using. **AWS SQS** seems to be
better since it offers a lot of functionalities around its SQS service, for example, mapping the
Message events to Lambda, which allows us to save a lot of time working on the ops side and focus
more on our business core value.

> Currently, we are running 2 different systems on 2 different Cloud providers and we are using both
> solutions.

<!-- more -->

Read more about Message Queue
[here](https://aws.amazon.com/message-queue/) and the Benefits of using Message Queue
[here](https://aws.amazon.com/message-queue/benefits/).

# A Webhook API backed by a Message Queue

> This method doesn't apply for other types of public API, mostly just webhook API

In our system, there are several public API endpoints we use to receive the updated data
from the other systems, including webhook API to receive email events from Mailgun,
integration API to receive changed customer data from third party partners that we integrate with.
There are 2 factors that we consider the most important for a public webhook API

- It should respond fast enough so we can handle a large amount of data update. Our system sends
  a lot of emails every day. There are a lot of events data sent from Mailgun and we are expected to
  capture all of them to. We also integrate with a lot of data from multiple insurance agencies
  across the US and it is required that we reflect the same data on those systems.
- Once the request has been confirmed, it should never lose the data until the data is successfully
  processed. No matter the code has bug or not, **eventually**, it should always process the data
  thoroughly and display to the user.

Those criteria match exactly with what a Message Queue can offer. Most of our public API endpoints
are backed by a Message Queue, which defers and ensures the success of data processing.

![API](/files/2020-03-15-message-queue/api.png)

Basically, the API server is designed as simple as possible. It performs only some simple
validations on the input data, make sure they follow the correct schema and then publish a message
with the request as the body to a queue. The API then responds back to the client immediately, with
a message promising that the data will be processed at some point in the future. In the background,
there is a worker that subscribes to this queue and consumes the messages. It is
responsible for processing all the complex logic related to the request data, for example, wrapping
data from multiple data sources, performs some CPU-intensive operations to convert the data to our
schema or handles activities logging.

There are some benefits of moving all the complex logic from the API server to the background
worker.
- Your public API is simpler. That means you can serve more requests and reduce the risk of
  downtime.
- The Message Queue ensures that all the data will eventually be processed successfully. Even if
  there is a bug in the code, we won't lose the data. The failed messages will go back to the queue
  (depending on the retry policy) or go to another Error queue, waiting for us to fix the bug and
  then retry the messages

# An Event Updater system backed by a Message Queue

This is the reverse of the above one. In previous section, a Message Queue is used to handled
updated data from other systems to our system. This section is about using a Message Queue to update
event back to the other systems.

The main product of AR (or FMGSuite now) is an Automation marketing platform. We do not provide any
CRM/AMS solutions to our clients. Instead, we try to integrate with as many popular products as
possible.
There are many cases that we want to write the data back to the those systems, for instance,
to write back the feedback of the customers of our clients or to insert a Prospect customer when a
user fill in a web form. The requirements are also quite similar

- The application should be fast enough so the user don't have to wait forever for a form to
  be submitted.
- The data must **eventually** be written to the other systems.

Here is a sample design of a Form submitting flow

![Form](/files/2020-03-15-message-queue/form.png)

Similarly, the user-facing API on the web app doesn't do any complex logic. It just tries to
validate and capture the user-input data as fast as possible then delegates the task to another
background worker. Of course, this design also brings some benefits compare to putting all the logic
in the web API

- Your web app will respond faster, the users will feel happier, the app looks more
  responsive.
- You will never lose the customer data. In case the 3rd-party system is down or any other network
  issues, the messages are still kept in the queue, retries multiple times until success or moved to
  the Error queue waiting to be re-queued at one point in the future.

# To be continued...
