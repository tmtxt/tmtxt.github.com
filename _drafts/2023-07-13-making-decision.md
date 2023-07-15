---
layout: post
title: "Don't make Http status codes confusing"
description: ""
categories: []
tags: []
thumbnail:
---

In software development, one of the most popular communication methods between systems is JSON http request.
It is also used in modern web development as a way to communicate between backend and frontend. It has 
been there for a long time, built primarily around status code, header data and body data. However,
I often find it confusing when seeing such API design with weird http status code.

Imagine you have an API to update data for a Customer entity, it would look like this

```
PUT /api/customers/:customerId

Body
{
  name: string;
  passportNumber: string;
  passportCountry: string;
}
```