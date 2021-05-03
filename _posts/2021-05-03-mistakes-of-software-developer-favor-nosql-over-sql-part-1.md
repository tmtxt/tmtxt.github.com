---
layout: post
title: "Mistakes of a Software Engineer - Favor NoSQL over SQL - Part 1"
description: "It doesn't mean I hate NoSQL. I just want to use the right tool for the right job..."
categories: [misc]
tags: []
thumbnail: /files/2021-05-03-mistakes-of-software-developer-favor-nosql-over-sql-part-1/sql-nosql.png
---

What is this? This is just something to summarize the mistakes that I found from my experience
as a Software Engineer.

The first one that I want to talk about is the war between SQL and NoSQL. This is one of my favorite
topic. I can debate about this topic for a whole day. 😁

> It doesn't mean I hate NoSQL. I just want to use the right tool for the right job...

This may be true at this time but who knows what will
change in the future? I have faced a lot of problems with the design of NoSQL, especially when it is
the main backing service for the whole system. Each database has its own strength and weakness and
we should use it for the most suitable job or at least, pick the one that can solve 80% of your
problems and live with the other 20% pain instead of choosing the one that fits for 20% use cases
and fix the problem that it brings to the other 80%.

> By NoSQL, I mean Document database systems (Mongo DB for example)

# A real life loop

Back in the 1970s, the most popular database used a simple data model called the hierarchical model,
which is similar to JSON model used in document databases today. That model worked well for
one-to-many relationships. However, when it comes to
many-to-many relationships, this design exposes a whole lot of problems, from joining issue to
duplicate data management. The two alternative solutions were proposed. They were Relational model
and Network model. The fact is obvious, Relational model wins the long run.

It seems that NoSQL
databases today are repeating the problems in the history, which have been solved for decades.
Despite the advantages of a more flexible data schema or a better representation of
data (which is more similar to objects in programming language), the 1970s' problems still exist and
developers still have to handle them manually in an awkward way.

> [Designing Data-Intensive Applications: The Big Ideas Behind Reliable, Scalable, and Maintainable Systems](https://www.oreilly.com/library/view/designing-data-intensive-applications/9781491903063/)

<!-- more -->

# The problem of Document Storage model

Usually in NoSQL, you are encouraged to store the object and its relations in one single
document instead of following the 3 forms of database normalization. For example, in an Insurance
AMS system, a Customer object also contains the list of all its contacts (spouse, parents or
children), the list of all the policies this customer has purchased and a collection of all the
claims this customer has made. This leads to a table consisting of multiple large documents. So what
is the problem here?

- What happens if you just want to read some fields of the Customer object (`firstName`, `lastName`
  or `emailAddress`)?
- What happens if you only want to update some fields of the Customer object?
- What happens if you only want to work with the Customer object itself, not the Policy, Claim or
Contact object?

Unless the fields you want to work with stays on the
[index tree](https://rethinkdb.com/docs/memory-usage#internal-metadata),
I cannot imagine there is any
solution to interact with each individual field in an optimal way. The database system has to load
the  whole document into memory to process, to read only some specific properties, to update only
some  fields and then write the whole document back to disk.

- What happens if your customer object contains a lot of policies? It can be a Commercial customer (a
  Company buying insurance for all its employees). The whole object will be loaded to memory just to
  get/set some fields.
- Even if you store small objects, what happens if you read/update just one field of millions of
  object? All related documents will be loaded into memory just to get some specific fields.

We had this problem in the past. We simply stored the objects without worrying about the size of each
record until we realized it was a big problem. All the solutions led to SQL design! The Document
Storage can cause performance problems for your application when you scale

- From my experience, except for some specific cases (which will be mentioned later), you rarely have
to do CRUD on the whole object. Each feature will require only some fields to work, not always the 
whole.
- As your application grows, there will be more properties added into your object. That will slow
down your query from time to time.
- You will soon have to fix that problem in an ugly way, either by creating a cloned version of the
table with only some necessary fields or use a SQL design.

> Why don't use SQL from the beginning?

# A Non-Relational world?

> **Relations** still exist in NoSQL, but...

You can still organize your entities into different tables (or collections in Mongodb), but
that's not the design philosophy of those Document database systems. They encourage you to combine
the related entities into just one single object, especially for one-to-many relationships. It best
fits for the case you mostly interact with the parent objects. The real world is a lot more
complex than that.

Take a look at this example. You are building an application to automate email marketing for your
insurance agency. Initially, the application targets the customers of the agency only. To support
that, you only need a table `customers` to store the customer objects with some basic fields. As your
application grows, the client requests to add some policies information into the email message (a
list of policies that the customer has purchased, for example). You will then update the objects in
that table to include a new array field called `policies`. That works fine and your business
keeps growing. Then you decide to add more features to work with Policy entity type. Now, you have 2
choices

The first option is to keep the current table structure, add more fields into the `policies` array
property of the `customer` object. This solution is backward-compatible, doesn't require any changes
to the current application code. However, you will soon realize the troubles. It will come back
to the problem of large objects that I mentioned above. It will affect the performance of all other
read/write operations. You will pay the penalty when interacting with only some fields of the
customer object. Another problem is that it's hard to work with the Policy objects directly. You
will always have to go through the parent customer object. To retrieve a single policy, you will
have to query the parent customer, load the whole object into memory (either database memory or
application memory), filter to the policy that you are looking for and then return it. To query the
list of policies and then sort by some conditiion, you have to query all the customer objects in the
system just to get the policy data and then aggregate them. Your application code logic will be more
and more complex, the performance of your application will keep decreasing.

Another way is to create a separate `policies` table to support for policy-related features. If you
keep the existing policy data in the `customers` table, you will have to deal with the duplicated data
problem. How to maintain the consistency between the 2 tables? That should be the database job,
shouldn't it? Now, it's likely that you have to maintain by your own on application level.
What happens if you instead decide to just store the policy data in the `policies` table? You will
have to update several places in your application code to match with the new data schema. It's not
also not recommended in Document databases. You will have to either use an ugly joining solution
from the database or do the join inefficiently yourself on application level. Why don't you just use
SQL to get all those benefits for free?

> As your business grows, you can never store everything inside one object. You will have to break
> down your objects into smaller ones and create the link between them. This is when you realize
> the importance of SQL

# Is that all?

No, part 2 is coming...

# Does that mean you should not use NoSQL?

Definitely NO, wait for part 2...
