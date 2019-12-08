---
layout: post
title: "Data Integration with AWS Lambda at high scale"
description: ""
categories: []
tags: []
thumbnail: 
---

So, the last 3.5 years of my life was a lot about Integration work (not everything, but a lot). I
have been working on Data Integration for the 2 systems at my company, one in Nodejs and one in C#
.Net Core. The one in Nodejs is based on Docker/Kubernetes architecture. We have spent a lot of time
building the all necessary tools around it, from the message queue helpers (persist message values,
topic/subscription to worker instance mapping,...) to the scaling mechanism (scale up, down, pause
specific worker,...). Coming to the AWS world and Lambda, we can apply many of our philosophy with
much lower effort. This post describes the overview of the architecture that we applied at FMGSuite
for Data Integration.

# Data Integration

**Data Integration** is an integral part of today Applications. It's much easier for a startup to
stand on the shoulders of giants. It's also easier to cooperate instead of competing. We are
building the automation marketing platform for our customers. To do Marketing, we need data (our
clients' customer data). We want to focus ok our core business value and we don't want to reinvent
the wheel to build another CRM (or AMS) system. This leads to the need for implementing the
integration with as many systems as possible. The more systems you can integrate, the more chance
you can get to win a deal. At **AR/FMGSuite**, I and my team are building the infrastructure to grab
our clients' customer data into our system. That includes syncing the customer list, the customer's
relationship, the customer's contact information,...

