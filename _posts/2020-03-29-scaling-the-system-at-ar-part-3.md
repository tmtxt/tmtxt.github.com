---
layout: post
title: "Scaling the System at AR - Part 3 - Message Queue in general"
description: "In previous post, I mentioned Messages Queue in some specific use cases for Integration components. In this post, I'm going to talk about Message Queue in general and how the workflow looks like at AR."
categories: [misc]
tags: []
thumbnail: /files/ar-logo-1.png
---

> Part 2:
> [Scaling the System at AR - Part 2 - Message Queue for Integration]({%post_url 2020-03-28-scaling-the-system-at-ar-part-2%})

In previous post, I mentioned Messages Queue in some specific use cases for Integration
components. In this post, I'm going to talk about Message Queue in general and how the workflow
looks like at AR.

# Messages Queue in design

One of the main difference of the AR system is that most of the tasks are background tasks and
backed by several Message Queues. There are several reasons for us to choose this design

- We want to keep the user-facing API and databases simple. This way, the API will respond
  very fast and the app performs more smoothly. That brings a good impression to our users and makes
  them happier.
- We can isolate different aspects of the system
  - We can easily limit the resources consumption of the less important tasks (the tasks that are
    not user-facing or do not need the results immediately), for example, the task to log User
    Activities or the task to export User Data.
  - We can also allocate more resources for and scale only the tasks that are critical to the users,
    for instance, the task to send a Blast email in case of disaster.
  - This is controlled by via various parameters when creating the queue and running the worker
    - The number of worker instances running at the same time
    - The number of concurrent messages that a worker instance can pull and process at the same time
    - The delay of the messages that are published to the queue
    - ...
- The Message queue ensures the eventual consistency for our system in case of failure. The system
  is fault-tolerant by design. Even if the database or the network is down, all the tasks are
  guaranteed to be processed at some points in the future. Moreover, we only need to retry the
  failed parts, not the full flow.
- Each worker is a re-usable workflow with the message value as the input data. Want to implement a
  new feature which re-use the same flow? Instead of activating the same functions, you publish a
  message to the corresponding queue.
- This allows us to choose different technology for each worker, depending on the requirements. Most
  of our workers are written in **Nodejs**. However, there are some of the written in **Golang**. We
  also have a team with many **C#** experts working on Integration projects. There are no problems
  for us to integrate everything into one same workflow.

<!-- more -->

# A Blast Email sending flow

Below is a simple example of a Blast Email sending flow, one of the main flow of our system.

> This feature is especially helpful for the Insurance market. Before each big event or holiday,
> they usually need to send out an email to their customers informing about the work plan of the
> Agency. Sometimes, in case of disaster, they also need to send out email to their customers
> to announce about the hotline or about the claim support so their customers feel safe about it.

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

Let's step back and see what Message Queue can help in each step

1. The Message Queue helps reduce the complexity in the Web API, make the API respond faster and
   make the app behave more smoothly.
2. All the important data is captured and ensured to be processed successfully in the future. If you
   put the logic in the Web API, there is the case it successfully sends to some contacts but fails
   to send to other contacts. This causes a problem for our users because they don't want to send
   multiple emails with the same content to any recipients.
3. Same with the above
4. This is the key to scale the performance of our system.
   - We can easily adjust the number of worker instances running at the same time (thanks to
     `Docker` and `Kubernetes` or `AWS SQS Lambda event source`) or the number of concurrent
     messages each instance should process.
     This means we are scaling only the necessary parts of our system, not the whole. Depending on
     the third party API limit and the capability of the databases that it touches, we will
     configure these numbers accordingly so it's fast enough but still in the maximum workload that
     the database can handle.
   - In case of failure (**Mailgun** is down or our database is down), we only need to retry
     specific customers, not the whole process.

There are other interesting things need to mention here
- Each component is independent (in term of technology/programming language). As mentioned before,
  there are workers written in C# and Golang in the AR system. Recently, there is a .Net team
  joining the product so we also implemented some other workers running in C# without any problem
  integrating them into the existing system.
- Each worker is reusable. What you need to do is to publish the same message data into the queue to
  activate the worker
  - We added the feature to send an email message directly to a specific customer simply by
    publishing the same message to the worker `BlastCustomerSender`
  - We also introduced another helpful feature, that is to schedule a Blast email in the future.
    What we need to do is simply let another timer worker to publish the same message to
    `BlastCustomerListSender` worker at the configured time.

Of course, there are many other aspects that a Message Queue can help to improve your system, for
example, act as an event-sourcing system, partition and allocate resources
for each different clients or temporarily disable one specific feature of the system. However, I
will leave it for you to examine...

# To be continued...
