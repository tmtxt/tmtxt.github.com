---
layout: post
title: "Scaling the System at AR - Part 3 - Message Queues cont."
description: ""
categories: [misc]
tags: []
thumbnail: /files/ar-logo-1.png
---

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
