---
layout: post
title: "Scaling the System ar AR - Part 1"
description: ""
categories: [misc]
---

As the time of this writing, I have been working at Agency Revolution (AR) for more than 2 years, on
a product focusing mostly on automation email marketing for the Insurance Agencies. I have been
working on this product since it was in beta, when it could only serve only a few clients, send
thousands of emails each month and handle very little amount of integration data without downtime
until it can deliver millions of emails each month, store and react to terabytes of data flow every
day. The dev team has been working very hard and suffering a lot of problem to cope with the
increasing number of customers that the sale team bring to us. Here are some techniques and
strategies that we have applied until now in order to bring a better user experience.

# Docker, Kubernetes and Microservices

Microservices can bring you both advantages and disadvantages, depending on the use case, you should
apply it in the correct way in order to gain the benefit, otherwise, you will have to pay other cost
that you couldn't imagine before. I wrote another post about some of the disadvantages in
Microservices here, feel free to take a look at
[The downsides of Microservices]({% post_url 2017-12-25-the-downsides-of-microservices-part-1 %}).
Although there are still many arguments about the benefits of using Microservices (and sometimes we
have to pay unnecessary cost for it), it has helped us scale the system in various ways, especially
when combining with Docker and Kubernetes.

Microservices helps us dividing the system into smaller components and we can scale the components
that require the most resources very easily, simply by setting the replicas configuration in
Kubernetes and it will take care of the rest. Kubernetes allows us to label all the servers and
schedule the corresponding services to their specific nodes. At the beginning stage when we still
had limited computing resources, we had to decide whether we need to prioritize these services more
than other services. We need to define a list of services 

# RethinkDB

# Google PubSub

# Pre-Compute

We used to face slowness problem.