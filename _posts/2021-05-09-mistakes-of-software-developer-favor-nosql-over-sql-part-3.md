---
layout: post
title: "Mistakes of a Software Engineer - Favor NoSQL over SQL - Part 3"
description: "It doesn't mean I hate NoSQL. I just want to use the right tool for the right job..."
categories: [misc]
tags: []
thumbnail: /files/2021-05-03-mistakes-of-software-developer-favor-nosql-over-sql-part-1/sql-nosql.png
---

> Part 2 [Mistakes of a Software Engineer - Favor NoSQL over SQL - Part 2]({%post_url 2021-05-08-mistakes-of-software-developer-favor-nosql-over-sql-part-2%})

# Is that all problems that I have with NoSQL?

No. But I'm lazy now 😂. So I won't talk about the problems anymore.

# Choosing a Database system with a Product perspective

So, how do I choose a database system from my Product engineer view?

> The answer is: It depends. (of course 😂)

The philosophies of the 2 database systems are different.

**NoSQL** is designed for simple operations with very high read/write throughput. It best fits if you
usually read/write the whole unstructured object using very simple queries. There are some specific
use cases that you can think about
- Receive data from another system (an integration API for example): You can use a NoSQL database as
  a temporary storage to increase API response time and safety by deferring processing data.
  Read more
  [Scaling the System at AR - Part 2 - Message Queue for Integration]({%post_url 2020-03-28-scaling-the-system-at-ar-part-2%})
- A backing service to store Message Values for a Message Queue application
- A caching database where you know exactly what you need and you can query easily by key value
  (but even in this case, think of other solution in SQL first, like materialized views,
  triggers,...).
- Others?

<!-- more -->

SQL, on the other hand, is built for complex data structure with efficient join ability. I would say
it is suitable for almost all the other cases 😂. If I am building a product, very likely,
my answer will be SQL.

- Most business will become more and more complicated, no doubt. You can never live without
  relational model or join ability. Let me repeat it: NEVER EVER.
- SQL can solve 80% problems that you face. For the other 20%, there is always workaround for them.
  Or you can apply NoSQL in those cases and gain the benefits of both worlds.
- With decades of development, there are enough tools, documents and solutions around the internet
  to support you. The technology that you use will hardly be abondoned and your product has a higher
  chance to survive (and save cost).

How about scaling factor? Somebody debate that NoSQL is designed with horizontal scaling from the beginning
and SQL, the reverse way, for vertical scaling only. However,

- Like I said before, there are some use cases in which one database can perform better than
  the other one. Let's them correctly and don't assume that there is a
  Swiss-Army-Knife database that can always operate well at high scale.
- I have seen many systems running SQL at very high scale, and they still work fine!
The problem sometimes is just the developers haven't reached the its limit yet.
- Even if NoSQL is better for scaling, it is not smart to choose a horizontal scaling solution at first.
Building a product, especially from the scratch, means you have reach out to the market and deliver
  values to the customers quickly. They don't care what kind of technology you use behind. They
  care about the product, the values that they receive when using your product. Horizontal scaling
  at an immature phase will just slow you down and leave the market for the other competitors
  (but we are lucky as there were no other competitors at the time we started 😁).
- Even if you can survive through the startup time, you should only think about horizontal scaling
when you have reached the limit of vertical scaling.
- Even when the team grows, it is a good idea to split into multiple ones to form several functional
  teams. Each functional team should be small enough and can operate independently to deliver customer
  values (from start to end). Cross team communication could be a practice of horizontal scaling
  but inside one team, do you really want/need horizontal scaling, do you really need to break
  things down to make it harder to manage? The answer is yours.

> I'm not for or against NoSQL. I just feel
> annoyed when seeing people too fancy about what is advertised about a new technology.
> They only show you the good side! Think and re-think carefully everytime you decide to choose any
> technology.

and always try to

> Use the right tool for the right thing!
