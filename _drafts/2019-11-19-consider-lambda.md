---
layout: post
title: "Things to consider before choosing Lambda"
description: ""
categories: []
tags: []
thumbnail:
---

> Lambda is not the key solution to all the problems. Lambda does not always reduce development
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
course, helps us a lot in managing infrastructure but it also adds a lot of headache to our
development workflow.

# Is your function Synchronous or Asynchronous?

This should always be the first question that you should ask when you considering Lambda as an
option for your stack. Personally, I don't think Lambda is a good fit for processing synchronous
tasks. Actually, it can! And there are several workarounds/techniques to help you cope with that.
However, they are not worth investing the effort in.

## Cold-Start Time

In the world of Lambda, the function is triggered on-demand and is be disposed it stays idle for a
period of time. That means the function is not always ready for use. When your function is invoked
for the first time after a while, it needs to download the code and bootstrap the runtime for your
application. It is called the `cold start` time. There are a lot of articles on the internet
comparing the cold start time of Lambda and the other cloud services or comparing among different
runtime options in Lambda. You can read the detailed comparison here
[AWS Lambda Cold Start Language Comparisons, 2019 edition](https://levelup.gitconnected.com/aws-lambda-cold-start-language-comparisons-2019-edition-%EF%B8%8F-1946d32a0244).
Of course, AWS has been working really hard to improve the `cold-start` time but it's the nature of
the serverless architecture. You cannot avoid it, you can only try to optimize it.

So, why do we need to care about `cold-start` time? Of course, `cold-start` time affects your API
performance (if you are building an API system). If your function is not used very frequently, there
is a high chance that it will be shut down very often. Whenever there is a new incoming request, AWS
has to perform all the provision steps again to bring that Lambda live and serve the request. This
is even worse if you use the languages that requires an extra compilation step (like `.Net`).
Everything behaves extremely slow for the first time. Hopefully this will be improved in the next
version of `.Net Core`.

## One request at a time

Yay, each Lambda instance processes only **ONE** request at the same time. Even if your function is
kept alive, there are also a high possibility that there are more incoming requests than the number
of available instances at that same time. As a result, all the other requests end up as waiting for
AWS to spin up a new instance of Lambda to process.

We usually workaround this setting up a timer job, sending some requests on interval, to keep the
minimum number of instances a live for some public API, but again, is it worth investing in?
