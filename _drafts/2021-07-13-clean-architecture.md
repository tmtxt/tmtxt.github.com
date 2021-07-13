---
layout: post
title: "Clean architecture in C#/.Net Core"
description: ""
categories: []
tags: []
thumbnail:
---

Nodejs has been a pain point in our code base for year. We started with Nodejs several years ago. It
is one of the best choice for starting a product because of the ease in development.

# Revisit Clean Architecture

Let's take a look at this very famous Clean Architecture demonstration image

![Clean Architecture](/files/2021-07-13-clean-architecture/clean-architecture.jpg)

Actually, for me, it was a bit hard to understand when I first read about this. I'd like to re-draw
it like this

![Clean Architecture](/files/2021-07-13-clean-architecture/clean-architecture.png)

- **Business layer** is where your application logic resides. It's pure C#, independent from the
  framework, database or any external dependencies.
- **Controller layer** is where we map out the underlying implementations of all the contracts (interfaces) we
  defined in **Business layer**.
- **Infrastructure** is actually related to the deployment and the operations of the other external
  cross-cutting concerns

Data flow: ....

Fact: naming is hard

# MediatR FTW

# Business Layer

# Controller Layer

# Infrastructure Layer
