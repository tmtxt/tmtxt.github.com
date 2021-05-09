---
layout: post
title: "Mistakes of a Software Engineer - Favor NoSQL over SQL - Part 3"
description: "It doesn't mean I hate NoSQL. I just want to use the right tool for the right job..."
categories: [misc]
tags: []
thumbnail: /files/2021-05-03-mistakes-of-software-developer-favor-nosql-over-sql-part-1/sql-nosql.png
---

> Part 2 [Mistakes of a Software Engineer - Favor NoSQL over SQL - Part 2]({%post_url 2021-05-08-mistakes-of-software-developer-favor-nosql-over-sql-part-2%})

# Is that all problems?

No. But I'm lazy now 😂. So I won't talk about the problem anymore.

# Choosing a Database system with a Product perspective

So, how do I choose a database system from my Product engineer view?

> The answer is: It depends. (of course 😂)

The philosophies of the 2 database systems are different.

NoSQL is designed for simple operations with very high read/write throughput. It best fits if you
usually read/write the whole unstructured object using very simple queries. There are some specific
use cases that you can think of NoSQL
- Receive data from another system (an integration API for example): You can use a NoSQL database as
  a temporary storage to increase API response time and avoid losing data (because of code bugs or
  network error,...). Read more
  [Scaling the System at AR - Part 2 - Message Queue for Integration]({%post_url 2020-03-28-scaling-the-system-at-ar-part-2%})
- A backing service to store Message Values for a Message Queue application
- A caching database where you know exactly what you need and you can query easily by key value
  (but even in this case, think of other solution in SQL first, like materialized views, trigger,...).
- Others...

<!-- more -->

SQL, on the other hand, is built for complex data structure with efficient join ability. I would say
it is suitable for almost all the other cases 😂. If I am building a product, most of the time,
my answer would be SQL.

- Most business will become more and more complicated, no doubt. You can never live without
  relational model or join ability. Let me repeat it: NEVER EVER.
- SQL can solve 80% problems that you face. For the other 20%, there is always workaround for them.
  Or you can apply NoSQL in those cases and gain the benefits of both worlds.
- With decades of development, there are enough tools to extend the functionalities as well as
to monitor effectively.

How about scaling factor? Somebody debate that NoSQL is designed with horizontal scaling from the beginning
and SQL, the other way, for vertical scaling only. However

- Like I said before, there are some use cases in which one database can perform better than
  the other one. Let's use the right tool for the right thing and don't assume that there is a
  Swiss-Army-Knife database that can always operate at high scale.
- I have seen many systems running SQL at a very high scale, and they still work fine.
The problem sometimes is just the developers haven't reached the its limit.
- Even if NoSQL is better for scaling, it is not wise to choose a horizontal scaling solution at first.
Building a product, especially from the scratch, means you have reach out to the market and deliver
  values to the customers very fast. They don't care what kind of technology you use behind. They
  care about the product, the values that they receive when using your product. Horizontal scaling
  at an immature phase will slow you down and leave the market for the other competitors.
- Even if you can survive the Startup period, only think about horizontal scaling when you have
reached the limit of vertical scaling.
- Even when the team grows, it is a good idea to split into multiple ones to build small functional
  team. Each functional team should be small enough and can operate independently to deliver customer
  values. Cross team communication could be an exercise of horizontal scaling but inside one team,
  do you really want/need horizontal scaling?

> I'm not for or against NoSQL. I just feel
> annoyed when seeing people too fancy about what is advertised about a new technology.
> They only show you the good side! Think and re-think carefully everytime you decide to choose any
> technology.

and always try to

> Use the right tool for the right thing!