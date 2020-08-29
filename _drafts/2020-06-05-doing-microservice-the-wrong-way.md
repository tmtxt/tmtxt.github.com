---
layout: post
title: "Doing Microservice the wrong way"
description: "or Considerations before moving to Microservices?"
categories: [misc]
tags: []
thumbnail: 
---

> or Considerations before moving to Microservices?

Over the last few years, I have seen the term **Microservices** emerges 

# Apply it at the wrong team size

You should always ask this question first before start thinking about applying Microservice. If you have at least 2-3 development teams, you can start thinking about this. Each team will be responsible managing one (or some) microservices, which can operate independently from each other. However, If you are a startup or if you have a small team, what are the benefits of applying Microservice? In that case, there is only one team responsible for managing all Microservices. Doesn't that sound like a Micro-Monolith?

Microservice adds a lot of overhead and engineering work just for maintaining the infrastructure. If you don't have a full time DevOps guy/team, never ever think about applying Microservice. If you have only one development team, please don't. If you work on your own, stop that idea ASAP.

# You haven't tried Vertical Scaling

- Who said Monolith could not scale?
- Who said Microservices could rolling-update without down time and Monolith couldn't?
- Who said multiple databases could serve better?

The basic idea behind Microservice is to divide the app into multiple independent small pieces, each responsible for only one business domain. The word **independent** here quite important. A service should have as few dependencies as possible. That means it should not depend on many other services.