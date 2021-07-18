---
layout: post
title: "Clean architecture with C#/.Net Core and MediatR - Part 1"
description: ""
categories: []
tags: []
thumbnail:
---

Nodejs has been a pain point in our code base for year. We started with Nodejs several years ago. It
is one of the best choice for starting a product because of the ease in development.
... continue here

# Clean Architecture revisit

Let's take a look at this very famous Clean Architecture demonstration image

![Clean Architecture](/files/2021-07-13-clean-architecture/clean-architecture.jpg)

To make it simpler, I will re-draw it a bit.

Here is how the data flow through the application

![Data Flow](/files/2021-07-13-clean-architecture/data-flow.png)

Here is how the Projects in C# look like. The outer project references the inner one

![Reference](/files/2021-07-13-clean-architecture/reference.png)

The arrows that I drew are also a bit different from the original Clean architecture flow but it's
actually the same. The Data flow is

- The core Business logic resides in **Business Layer**. They are pure C#, independent from the
framework, database or any other external services. It doesn't care if one request comes from an
Http API, from a Timer Worker or a Script.
- The framework outside needs to wrap the request, transform it into the same input format that the
**Business Layer** expects.
- The **Business Layer** also contains a list of interfaces to interact with all the external
dependencies (anything outside of the C# applications, from the storage layer to external services).
The implementations will be delegated to the outer circle to make the **Business Layer** become
loose-coupling with all those external dependencies.

Fact: **Naming** is hard. I don't know if the above names are correct. They are not even the ones
that I used in my code :joy:. Well, names are just names, as long as we understand what they do,
that's enough.

Fact: You may have more than 3 layers. They are just for demonstration purpose

# Business Layer
