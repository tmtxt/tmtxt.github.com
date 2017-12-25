---
layout: post
title: "The downsides of Microservices - Part 1"
description: ""
categories: []
tags: []
thumbnail: /files/2017-12-25-the-downsides-of-microservices-part-1/thumb.png
---

It has been nearly 2 years since I started working at Agency Revolution, a team working on a
software platform that utilizes Microservices architecture to build a highly scalable system for
Automation Marketing. It comes with both pros and cons when building a Microservices system from
scratch and I’m not for nor against Microservices. There are many articles and books on the Internet
talking about the advantages of Microservices so I'm not going to write another post about the
benefits of using Microservices. This post is just a summary of my experience and the difficulties
after 2 years working with it as well as how we deal with those issues to get the most value of
Microservices.

First, let me introduce a bit about the tech stack that we are using. We have been running our
application on our private server for about 2 years before migrating to Google Cloud Platform. There
are 3 types of service in the system. They are

- **HTTP services**: the services for handling synchronous requests, the requests that need the response
immediately (e.g. requests from frontend to display for users)
- **Google PubSub workers**: the services for handling asynchronous requests. They are queued for later
processing in the background and ensured by Google PubSub
- **Timer workers**: The services that run at intervals.

**HTTP services** are used for handling simple requests, which can be completed within
milliseconds/seconds. For the long-running tasks, we published a message to Google PubSub and
schedule it to be processed later by the **Google PubSub workers**. Each of them is deployed and
scaled as a pod in Kubernetes.

# First problem - Overhead before you can get the scalability to work

Microservices add quite a lot of unnecessary overhead in the beginning, both human and computing
resources.

### Computing Resources overhead

One thing that you have to pay for is the performance cost of starting a lot of small
processes. If your resources are limited, this can slow down the whole system. You need to think
carefully when you start with a full Microservices system from scratch whether you really need to
break down the whole system into smaller parts, whether you have enough computing power or how large
you will scale your application.

<!-- more -->

Imagine that there are about 10 modules in your system for 10 different domain business. If you are
building a Microservices system, you will probably split your application into many smaller
services, one for each domain. Each of them may be implemented using a simple HTTP JSON Api server.
Instead of calling the exposed functions each module provides like in the monolithic design, you will
have to initiate an http request calling to the corresponding service, get the response data and
continue processing. The problem arises when there are many modules/services that are rarely used,
just serve a few requests per day. You still have to keep those http web servers alive to listen and
process api requests. What happen when you system grows to 100 services (or more)? How many of them
will actually be used/stressed all the time? Think about the isolated backing services used for each
service. All of them still consumes your CPU/RAM even if they serve no requests. A monolithic
application can easily share the computing resources among all the modules.

We ran into this situation when our application grew to about 100 small services. We had to keep
all the HTTP services and Google PubSub workers alive to handle HTTP requests and process messages
from Google PubSub. Some workers are extremely rarely used, for example, the worker to delete one
customer (runs just once or twice every month). We wasted a lot of resources keeping those small
services running just to wait for incoming messages/requests. We solved this issue partially for the
Google PubSub workers by implementing a Workers Scheduler service. It is a **Timer worker** that keeps
track of messages published to Google and send requests to Kubernetes master server to create new
pods and destroy running pods based on some predefined rules.

### Data Transferring cost

Another overhead is the cost of transferring data through network. In a monolithic design, each
function call is simply passing the reference to the real object through parameters. There is no
need for cloning the object itself. However, communication between difference services requires
serializing, deserializing the whole object and transfer it over the network. This can lead to a
major performance issue if cross-service communication happens frequently. To solve this, you will
need filter only the necessary fields before sending the object to the next service. Also, your API
need to be designed in a way that allows the caller to specify the fields to return to reduce
network transferring.

There are some other solutions that I believe they can improve the performance for encoding/decoding
objects through services like [FlatBuffers](https://google.github.io/flatbuffers/)
or [Protobuf](https://github.com/google/protobuf). I will come back to write another post about the
comparison once I finished integrating one of them in the architecture.

### Human Resources overhead

You also need to invest a lot of human resources in devops and setting up production pipeline in the
beginning to automate the deployment of all services (yeah you can not deploy hundred of services
manually!). You have to spend a lot of time on defining a standard interface for communicating
between services because you cannot initiate all the remote api calls manually every time.

Testing is also a complex process, deciding whether to go with unit test in each service or
integration test where one service would require many other services to be started are really
matter. You can decide to mock all the dependent services but you will have to repeat this step
many times if one service is consumed a lot by many others and update all the mocks when
there are breaking changes in that service. You can choose another approach to start all the
dependent services along with the one you need to test, but it's also increase the overhead of
setting up and making sure everything works together properly and automatically.

# To be continued...
