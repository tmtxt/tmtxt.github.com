---
layout: post
title: "Why people are still so addicted in NoSQL?"
description: ""
categories: [misc]
tags: []
thumbnail:
---

I have been working on a system which uses RethinkDB (a NoSQL) database system as its backing service for almost 4 years. There are a lot of cool optimizations we have applied
(read ...) that reach the state of the art of RethinkDB and help us scale the system to serve our users better. I personally didn't imagine we could scale that big with a schemaless database but we did! The system now delivers millions of email every month, automates many repetitive boring marketing tasks, provides a detailed insight to the customer activities and help our users grow their business through a better automation marketing platform. However, my overall experience with NoSQL has been very bad, especially when we combine with a dynamic programming language like Nodejs. We have reached its limit and it started exposing problems

# The problem of large objects

Usually for NoSQL, you are encouraged to store the object and its relationship in one single document instead of following the 3 forms of database normalization. For example, for an Insurance AMS system, a customer object would also contain the list of all its contacts (spouse, parents or children), the list of all the policies this customer has purchased and a collection of all the claims this customer has made. This leads to a table consisting of multiple large documents. So what is the problem here?

- What if you only want to read some fields of the customer object (`firstName`, `lastName` or `emailAddress`)?
- What if you only want to update some fields of the customer object?

Unless the field you want to work with stays on the index tree, I cannot imagine there is any solution to interact with individual field in an optimized way. The database system has to load the whole document to memory to process, to read only some specific props, to update only some fields and then write the whole document back to disk.

- What if your customer object contains a lot of policies? It can be a Commercial customer (a Company buying insurance for all its employees). The whole object will be loaded to memory just to get/set some fields.
- Even if you store small objects, what will happen if you read/update just one field in millions of object? All related documents will be loaded into memory just to get some specific fields.

We had this problem in the past. We simply store the objects without worrying about the size of each record until we realized it was a big problem. All the solutions would lead to a SQL design!

Why don't we just use SQL?

# The problem of Relations

When you app grows, can you avoid the relation of your entities?

# The performance cost

Some people debate that NoSQL databases are better used for heavy-read applications. The
argument is that reading the whole document object is much faster than reading through many fields
of one row (including the null fields). I cannot


# Schemaless doesn't Reduce the complexity or Save your development time

# The problem of Relationship

# The lack of Transactions

# Schema changes - Migration

For me, this is a nightmare when working with NoSQL.

# NoSQL and Dynamic languages will blow your mind

# Where NoSQL would fit?

# Summary
