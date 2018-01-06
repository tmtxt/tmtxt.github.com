---
layout: post
title: "The downsides of Microservices - Part 2 - Distributed system problems"
description: ""
categories: [misc]
tags: []
thumbnail: /files/2017-12-25-the-downsides-of-microservices-part-1/microservices.png
---

In the first [post]({%post_url 2017-12-25-the-downsides-of-microservices-part-1%}), I discussed
about the overhead that you have to pay for when working with Microservices. This time, I'm going to
talk about another problem with Microservices. It is the problem of the distributed systems that you
have to face with from the very beginning.

![thumb](/files/2017-12-25-the-downsides-of-microservices-part-1/microservices.png)

# You have to deal with the problem of Distributed systems very early

## Handling Data Inconsistency

A distributed system with a lot of small services followed by difference data storages means that
there are no constraints between those data storages. In a traditional SQL database, this can be
solved easily by adding foreign keys between tables and perform a cascading update/delete whenever
you want to modify the data. Ensuring that constraint in a Microservices design is really
challenging.

<!-- more -->

Let’s say you are building a blogging system using Microservices design. Every time a user is
deleted, all posts published by that user are expected to be removed, too. What happens if the
request just finished removing the user from user storage but failed to clear the related posts due to
network failure? The other users can still see the posts belong to a deleted user. You can
workaround this by sending a message to Kafka, Google Pub/Sub when the api request fails to process
and schedule an async worker to continue the work. Those messaging systems will ensure the message
is retried until success. You can also perform an extra check when getting the blog posts but yeah,
this will increase the response time of the api.

## Everything can fail, especially the Network

Communication between services can happen within the same server or more usual through the network
across multiple servers and the network can always fail. You need to handle error in case of network
failure, which you don’t have to care much when working with a single application. The caller
service has to take care of retrying fail requests and handle data inconsistency resulted by partial
processed requests. Your application need to be equipped with many timer workers to check and fix
data to ensure eventual consistency.

Backing your application by many timer workers is not easy and not a good workaround. It increases
the cost for development and maintenance since you have to take care not only the application code
itself but also the fixer workers. Sometimes the fixer workers cause data inconsistency more serious
because they don’t follow the updated schema.

## The problem of Error Handling, Logging, Tracing and Debugging

Tracing and debugging errors in Microservices are really a nightmare if you don’t spend time
building and setting up the tools around it. The most significant problem is to find the root cause
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
Microservices. Since we are using http for transferring data between services, that uuid can be
easily attached to the request header and processed by a logging middleware in each service. By
indexing that uuid field in ElasticSearch, we can easily filter out all requests having that same
uuid.

Later, I will write another post with detailed information about the logging and monitoring
architecture as well as how we do all the tracing and debugging tasks.

# Conclusion

In summary, you don't get much benefits for a small application to be designed using Microservices
architecture. You can start with a monolith design, try to optimise it as it grows bigger, try to
scale the application and database servers until the bottleneck cannot be handled by just adding
more resources. That is a suitable time for you to gradually switching your application to
Microservices design. You can choose the most stressed module to split to another application and
continue converting the other ones when necessary.

I have seen many people falling in love with Microservices and trying to apply Microservices design
anywhere they can. However, for me, it is just not right. I believe that each design has its own
strengths and weaknesses and we should decide which one to go with depending on the situation.
