---
layout: post
title: "Utilize RethinkDB Indexes"
description: ""
categories: [misc]
tags: []
thumbnail:
---

At Agency Revolution, we utilize RethinkDB as our main storage. Nearly everything is stored in
RethinkDB. Probably at the time you are reading this article, that's not true anymore. However, as
it's still one of our main data storage, we used to have a lot of performance issues related to
storing and retrieving data (and we still have until now). This blog post is to summarize how we
utilize RethinkDB indexes to solve those problems as well as some use cases where indexes can hurt
the performance.

Before you continue reading this post, there is one thing that I have to say. I hate the idea of
relying everything on a NoSQL database. Each database has its own strengths and weaknesses and
storing everything in RethinkDB is not a very good solution.

# Some rules when using RethinkDB Indexes

RethinkDB Indexes, similar to Indexes in other database, are the trade-off between read and write
performance. Therefore, the basic rules for RethinkDB Indexes are similar to other database.

- Don't create index if not necessary.
- Don't create index if the data set is small enough so that you can use the `filter` query.
- Indexes require memory to process, be careful with tables that have a lot of indexes.
- Indexes can slow down write operations significantly.

# RethinkDB Primary Index

Yes, it's the simplest index solution that you have by default for all table.
