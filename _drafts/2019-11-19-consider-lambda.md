---
layout: post
title: "Things to consider before choosing Lambda"
description: ""
categories: []
tags: []
thumbnail:
---

> NO, Lambda is not the key solution to all the problems. Lambda does not always reduce development
> time. Lambda does not always save your head from all the infrastructure problems. Lambda does not
> always scale well. It is not always true that you don't have to manage anything, just focus on the
> business logic. It is not always cheaper than traditional "server-full" architecture.

**There are truth that they never tell you. You have to learn it the hard way!**

Don't get me wrong. I don't mean Lambda is bad. I don't mean we should not use Lambda. It has its
own use cases and we need to understand correctly and apply it to the correct use cases. I have seen
some guys impressed by the ease of developing application using Lambda (as is advertised) and tried
to move the whole stack to Lambda. This is not wrong, but for me, it's simply over-engineer. Most of
them ended up by moving back parts of that out of Lambda.

So, what should you consider before choosing Lambda? What are the questions that you should ask
before moving to Lambda? Here are some things, concerns that you should take a deep look and answer
before entering the world of Lambda. They are based on my experience after 1.5 years working
with Lambda. We have built several systems based on AWS Lambda architecture, including
[an industry-first email reconciliation system](https://aws.amazon.com/blogs/startups/how-fmg-suite-reduces-risk-for-financial-institutions/),
a high-scalable **Contacts Integration** system for FMGSuite marketing platform, etc. **Lambda**, of
course, helps us a lot in managing infrastructure but it also added a lot of headache to our
development workflow.
