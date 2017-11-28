---
layout: post
showtn: yes
title: “Microservices - A look back after 2 years”
description: “”
category: misc
thumbnail:
tags: []
---

It has been nearly 2 years since I started working at Agency Revolution, a team working on a
software product that utilizes Microservices architecture to build a highly scalable system for
Automation Marketing. There are both pros and cons when building a Microservices system from scratch
and I’m not for nor against Microservices. This post is just a summary of my experience after 2
years working with it, what Microservices design helped our team in developing application as well
as the pain that we have to face when choosing that approach.

# What is Microservices?

Well, there are a lot of posts out there on the internet that describe what exactly Microservices design is.
#
# The good

## Easy to adapt with changes

One of the biggest advantage of Microservices is that it’s very easy to adapt with changes. You can
make changes in one small service without affecting other services as long as you keep the same API
specs. In case you are going to make breaking changes, just add a new v2, v3,... api and switch the
corresponding services consuming that api to use the new version. Small services also allow you to
release new features or bug fixes faster. You don’t

## Scale just what you need

## New team member can start working on some small services first

#  ...and the pain

> The “pain” here doesn't really mean the disadvantages of Microservices. It is the difficulties
> that we faced during the process of developing our application using Microservices design as well
> as some techniques that we used to overcome the problem.

## Overhead before you can get the scalability to work

Microservices add quite a lot of unnecessary overhead in the beginning, both human and computing
resources.

One thing that you have to pay for is the performance cost of starting a lot of small
processes. If your resources are limited, this can slow down the whole system. You need to think
carefully when you start with a full Microservices system from scratch whether you really need to
break down the whole system into smaller parts, whether you have enough computing power or how large
you will scale your application.

Imagine that there are about 10 modules in your system for 10 different domain business. If you are
building a Microservices system, you will probably split your application into many smaller
services, one for each domain. Each of them may be implemented using a simple HTTP JSON Api server.
Instead of calling the exposed methods each module provides like in the monolith design, you will
have to initiate an http request calling to the corresponding service, get the response data and
continue processing. The problem arises when there are many modules/services that are rarely used,
just serve few requests per day. You still have to keep those http web servers alive to listen and
process api requests. What happen when you system grows to 100 services (or more)? How many of them
will actually be used/stressed all the time? Think about the isolated backing services used for each
service. All of them still consumes your CPU/RAM even if they serve no requests. A monolith
application can easily share the computing resources among all the modules.

Another overhead is the cost of transferring data through network. In a monolith design, each
function call is simply passing the reference to the real object through parameters. There are no
need for cloning the object itself. However, communication between difference services requires
serializing, deserializing the whole object and transfer it over the network. This can lead to a
major performance issue if cross-service communication happens a lot. To solve this, you will need
filter only the necessary fields before sending the object to the next service. Also, your API need
to be designed in a way that allows the caller to specify the fields to return to reduce network
transfering.

You also need to invest a lot of human resources in devops and setting up production pipeline in the
beginning to automate the deployment of all services (yeah you can not deploy hundred of services
manually!). You have to spend a lot of time on defining a standard interface for communicating
between services, how one service depends on another service, how to do the testing, whether to go
with unit test in each service or integration test where one service would require many other
services to be started.

## You have to design with failure in mind.

Communication between services can happen within the same server or more usual through the network
across multiple servers and the network can always fail. You need to handle error in case of network
failure, which you don’t have to care much when working with a single application. The caller
service has to take care of retrying fail requests and handle data inconsistency resulted by partial
processed requests. Your application need to be equiped with many timer workers to check and fix
data to ensure eventual consistency.

Backing your application by many timer workers is not easy and not a good workaround. It increases
the cost for development and maintainance since you have to take care not only the application code
itself but also the fixer workers. Sometimes the fixer workers cause data inconsistency more serious
because they don’t follow the updated schema.

## You have to deal with the problem of distributed systems very early

A distributed system with a lot of small services followed by difference data storages means that
there are no constraint between those data storages. In a traditional SQL database, this can be
solved easily by adding foreign key between tables and perform a cascading update/delete whenever
you want to modify the data. Ensuring that constraint in a microservice design is really
challenging.

Let’s say you are building a blogging system using Microservices design. Everytime a user is
deleted, all posts published by that user are expected to be removed, too. What happens if the
request finished removing the user from user storage but fail to clear the related posts due to
network failure? The other users can still see the posts belong to a deleted user? You can
workaround this by sending a message to Kafka, Google Pub/Sub when the api request fails to process
and schedule an async worker to continue the work. Those messaging systems will ensure the message
is retried until success. You can also perform an extra check when getting the blog posts but yeah,
this will increase the response time of the api.

## The problem of logging and tracing/debugging

Tracing and debugging errors in Microservices are really a nightmare if you don’t spend time
building and setting up the tools around it. The most significant barrier is finding the root cause
of one issue when the logic is spread into multiple places.

Back to the blogging example above, if you want to implement an api to get all the comments belong
to one post, including the information of the users who commented. That api request need to go
through many different services, from the BlogPostService (to get post comments) to UserService to
retrieve all the related user information. If your users are organised into group (company), then
your application need to go through the CompanyService to get some related information about that
organization (depending on your business logic). Finally, the gateway service (the one which
receives the requests from frontend), we call it ApiServer here, need to do some computation to
combine all the data it got and send back to client. What happen if the request ends up with
incorrect data? Where can the root cause come from, the BlogPostService, the UserService, the
CompanyService or the ApiServer? How can we quickly retrieve all the logging data related to that
one specific request?

At AR, we have built a complete solution for logging and tracing the request life cycle with the
help of ElasticSearch and Kibana. Each request is associated with one random uuid. That uuid will be
maintained through the request life cycle and will be sent along with the request to all related
microservices. Since we are using http for transfering data between services, that uuid can be
easily attached to the request header and processed by a logging middleware in each service. By
indexing that uuid field in ElasticSearch, we can easily filter out all requests having that same
uuid.

Later, I will write another post with detailed information about the logging and monitoring
architecture as well as how we do all the tracing and debugging tasks.

#

You don’t get much benefit for a small application to be designed using Microservices architechture.
You can start with a monolith application, try to optimise it as it grows bigger and bigger, try to
optimise, scale the application and the database server until the bottleneck cannot be handled by
just adding more instances,
