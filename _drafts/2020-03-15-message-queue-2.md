---
layout: post
title: "Scaling the System at AR - Part 3 - Message Queues cont."
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

A message queue is an asynchronous inter-service communication pattern. It is a temporary place to
store the data, the operation, waiting for the message receiver to process. It encourages decoupling
logic and components in the system, provides a lightweight and unified protocol for communication
between different services (written in different languages) and is perfectly suitable for
Microservice design. A good message queue must satisfy these criteria

- It must be fast and capable of handling a large amount of messages coming in at the same time.
- It have to ensure the success of message processing. A message must be processed and retried until
  success. Otherwise, an Error queue (Dead Letter Queue) should be provided to store the failed
  messages later processing.
- It is required that each message is processed by one and only one consumer at the same time.
- The message queue should be independent from any languages and allow different applications
  written in different languages to send and receive messages without any problem.

Because the Message queue is so important to us and there is a limit in number of developers, we
decided to switch third-party services after several months doing both dev and ops work with Kafka.
**Google Pub/Sub** and **AWS SQS** both offer the service in a relatively cheap price and it worth
investing in.

> Currently, we are running 2 different systems on 2 different Cloud providers. That's why we are
> using both solutions.

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

![Form](/files/2020-03-15-message-queue/form.png)

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
- We can isolate different aspects of the system
  - We can easily limit the resources consumption for the less important tasks (the tasks that are
    not user-facing or do not need the results immediately), for example, the task to log User
    Activities or the task to export User Data.
  - We can also allocate more resources for and scale only the tasks that are critical to the users,
    for instance, the task to send a Blast email in case of disaster.
  - This is controlled by via various parameters when creating the queue and running the worker
    - The number of worker instance running at the same time
    - The number of concurrent messages that a worker instance can pull and process at the same time
    - The delay of the messages that are published to the queue
    - ...
- The Message queues ensure the eventual consistency for our system in case of failure. The system
  is a fault-tolerant by design. Even if the database or the network is down, all the tasks are
  guaranteed to be processed at some points in the future. Moreover, we only need to retry the
  failed parts, not the full flow.

Below is a simple example of a Blast Email sending workflow

![Blast](/files/2020-03-15-message-queue/blast.png)

1. A user clicks a button to trigger sending a Blast message to a list of customers
2. The Web App captures all the basic data, for example, the list of customer id, the email
   template and the custom message. Instead of processing the Blast email directly, the Web App
   publishes a message to the queue `BlastCustomerListSender` to handle the task to that worker.
3. The worker `BlastCustomerListSender` validates the status of the current user, whether his account is
   currently active and has enough credit to perform these tasks. It also computes the list of
   customers to send email to, distinct the list to save resources. It then publishes multiple
   messages to the next queue (`BlastCustomerSender`), each message corresponds to one customer that
   will receive the email.
4. The worker `BlastCustomerSender` is responsible various tasks related to sending email to one
   customer.
   - First, it validates the current customer status, whether this person is still an active customer.
   - It then checks whether this person wants to receive the email or whether he has been opted out.
   - It also validates the current destination email status, whether it is a valid email (using
     Kickbox), whether the email address is currently bound or not. This is really
     important in designing an email automation marketing system. We need to be careful so that our
     servers are not listed in the blacklist of other email providers.
   - It renders the custom email template specific for this customer (because we personalize the
     messages to fit our recipients).
   - Next, a request will be sent to Mailgun to trigger actually send the email.
   - Finally, the worker logs an Activity record the current customer.
   - This is the key to scale the performance of our system. We can easily adjust the number of
     worker instances (thanks to `Docker` and `Kubernetes`) or the number of concurrent messages to
     be processed at the same time. In case of failure (**Mailgun** is down or our database is
     down), we only need to retry specific customers, not the whole process.

The above example demonstrates some basic capabilities of what a Message queue can do and how it can
help you in designing and scaling the system. There are many other applications. You can improve it
a bit to transform it to a system that support event-sourcing.

Currently, we have over 100 different types of
workers/queues. We have also built a UI tool for managing them efficiently

![MessageBus](/files/2020-03-15-message-queue/message-bus.png)
