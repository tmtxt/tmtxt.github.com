---
layout: post
title: "Transition"
description: ""
categories: []
tags: []
thumbnail:
---

> We are transitioning (part of) our Nodejs Microservice system to .Net microservices.

> And yes, it is Microsoft, the company (and the OS) that I always wanted to stay away from. But Microsoft has changed now, it is not the old Microsoft anymore.

# Why?

Why transitioning? Why .Net? Why not another programming language? And why this blog post?

Starting with the first question, why did we move (part of our system) away from Nodejs? For us, the JS ecosystem is not reliable, it hasn’t evolved to the state id the art yet. The problem with JS community is many of the libraries/frameworks out there are not stable, not maintained well enough, not designed with careful thought at first. They change so frequently, they experiment things, give it for the community to test and then they break things, they introduce breaking changes, they abondon the projects, they move away,... There are a lot of standards out there to follow. It is good to compete to be the best, but for JS, there are just too many.

> I don’t mean JS/NodeJS is bad or the the whole community behaves that badly but it’s really a pain point with this community. I still enjoy writing JS code and work with it everyday.

Also, due to the nature of Nodejs, its biggest strength is dealing with IO concurrency, not with CPU intensive tasks. (but C# async/await also provides the same benefit). Scaling up a Nodejs application means to spawn another exact same instance, which take up the same amount of memory/cpu just for loading the code base, the necessary libraries and pay for all those overhead even. You cannot take advantages of the multi-core CPUs to do heavy computation work or to serve more requests. There is a limit in what Nodejs can do.

