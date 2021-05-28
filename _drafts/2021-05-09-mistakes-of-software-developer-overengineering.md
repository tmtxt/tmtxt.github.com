---
layout: post
title: "Mistakes of a Software Engineer - Overengineering - Part 1"
description: ""
categories: [misc]
tags: []
thumbnail:
---

Right, Overengineering! This is a very broad topic. I don't want to talk about what it is, how to
avoid it. I just try to document some examples that I found in real life as a sign of
Overengineering so every time I work on something, I know if I'm going in an Overengineering
direction.

# A multi-purpose API

For me, I always try to avoid designing a multi-purpose API. We all know (or have an idea of) this.
However, sometimes, we just too focus on implementing the logic without noticing what is wrong. So
how a multi-purpose API looks like? Here are some indications of a multi-purpose API. Notice that
they are not always true, there will be exceptions (will be mentioned later)

**Nested objects**

If your API is designed to insert/update nested objects, probably you are doing it incorrectly.

```
# insert customer API
POST /api/customers
Request body
{
  name:       <string>,
  age:        <number>,
  occupation: <string>,
  birthdate:  <datetime>,
  cellphone:  <string>,
  email:      <string>,

  addresses: Array<{
    line1:   <string>
    line2:   <string>
    city:    <string>
    state:   <string>
    zipCode: <string>
  }>
}

# update customer API
PATCH /api/customers/:customerId
{
  name:       <string>,
  age:        <number>,
  occupation: <string>,
  birthdate:  <datetime>,
  cellphone:  <string>,
  email:      <string>,

  addresses: Array<{
    line1:   <string>
    line2:   <string>
    city:    <string>
    state:   <string>
    zipCode: <string>
  }>
}
```

![Design](/files/2021-05-09-mistakes-of-software-developer-overengineering/multi-purpose-api-1.png)


If you design an API for 10 goals, very
likely that it won't work the way you expect. If you design just 1-2 APIs, each one is for one
specific task, you will get less chance that they will behave unexpectedly.
