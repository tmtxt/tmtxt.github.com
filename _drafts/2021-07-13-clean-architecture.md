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

Actually, for me, it was a bit hard to understand when I first read about this. I'd like to re-draw
it a bit

If we draw it to see the data flows

![Data Flow](/files/2021-07-13-clean-architecture/data-flow.png)

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
