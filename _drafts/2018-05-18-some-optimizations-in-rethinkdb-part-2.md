---
layout: post
title: "Some Optimizations in RethinkDB - Part 2"
description: ""
categories: [rethinkdb]
tags: [rethinkdb]
thumbnail: /files/2017-10-08-utilize-rethinkdb-index-part-1-primary-key-index/thumbnail.png
---

**Part 1 here**
[Some optimizations in RethinkDB - Part 1]({% post_url 2018-03-10-some-optimizations-in-rethinkdb-part-1 %}).

> Yes, it's RethinkDB, a discontinued product. Again, read my introduction in the previous post.
> It's not only about RethinkDB but it also the basic idea for many other database systems.

# Increase the Memory

Hmm, sound like a very obvious solution?

# Quick flush vs Late flush 

# Temporary data, scale with K8S

> This is a trick, work around not only for RethinkDB but also all the other database systems.

Since we are running the application inside Docker, deployed in Kubernetes on Google Cloud, we have free auto-scale ability from Google. What does this mean in scaling and optimising the database? Applications running in Docker should be stateless as much as possible. All the data should be safely cleaned after the container was killed. Well, there will be the case that all you need is to store a large amount of data temporarily for a shot period of time and K8S pods will be a good choice for this.