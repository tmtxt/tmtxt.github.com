---
layout: post
title: "The importance of Designing a stateless application"
description: "or How 12-factor app helps you build better software"
categories: [misc]
tags: []
---

> First thing first: What is **12-factor App**?

It's a set of techniques to build Web and Saas app. It's not the "Swiss Army Knife" solution for
every scenario but it’s becoming more relevant in today's Cloud and Container world.
The website [The Twelve-Factor App](https://12factor.net/) 

> First, let's review [Rule 6 of The Twelve-Factor App: Processes](https://12factor.net/processes) - Execute the app as one or more stateless processes

The rule states that your application should have no state at all. Any data must be persisted in one of your backing services. A stateful backing service could be a database, a cloud storage service (GCP Bucket or AWS S3) or even a message queue.

Here are some examples about an non-stateless app
- Sticky session, which caches user session in memory
- Any in-memory data structure in your app server. It could be a sqlite database embedded inside your app server or a simple set of objects in app memory that’s persisted and mutated across multiple requests to the server

