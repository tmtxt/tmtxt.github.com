---
layout: post
title: "Mistakes of a Software Engineer - Favor NoSQL over SQL - Part 2"
description: "It doesn't mean I hate NoSQL. I just want to use the right tool for the right job..."
categories: [misc]
tags: []
thumbnail: /files/2021-05-03-mistakes-of-software-developer-favor-nosql-over-sql-part-1/sql-nosql.png
---

> Part 1 [Mistakes of a Software Engineer - Favor NoSQL over SQL - Part 1]({%post_url 2021-05-03-mistakes-of-software-developer-favor-nosql-over-sql-part-1%})

# There is nothing called Schemaless

One argument that people usually make about NoSQL is the flexibility in schema designing. They
are Schemaless databases and very similar to objects in programming language. The dynamic schema is very
powerful and can support several different use cases, not limited like what SQL provides. You can
simply throw your objects into it and do anything you want.

> Is it really true? For me, it's **NOT**.

There is nothing called Schemaless. The schema just moves from Database level to Application level.
For Document databases, you are actually using implicit data schema model (compare to explicit data
schema in SQL). Instead of letting the database handle the schema and datatype itself, you have to
do it yourself in your application code.
- Some databases support simple schema validation. However, none of them are as optimal as what
  SQL provides. You are limited to just some basic data types (string, number, boolean,...) while
  SQL offers a wide range of different data types for different use cases (for example: varchar, nvarchar
  text,... just for string data). Working with strict data type is always easier and more performant.
- In case you handle the schema yourself, it **may** be ok if you use a static-typed programming language
  (like C#). However, if you are developing your application using Nodejs, Ruby, PHP,... (which is quite
  common for Startup companies), you have to maintain another layer of schema validation in your application
  because the data is just runtime objects.

Schema migration is also another problem. NoSQL databases encourage you to just throw the objects
into and not care much about the schema. Some databases don't even require you to create any table, simply
write the data and the tables will be created automatically.
When you read/write the data, it is hard to know which version the data is on. You will
have to add many `if` statements just to check for what should have been provided. No matter
how careful you design your application, how strict your coding conventions are, there will always be the
case your code works with outdated data schema. For critical business workflows, I want
everything to be expressed clearly, not implicitly.

<!-- more -->

# You are encouraged with bad patterns

> Not really a database issue, it's more about developer mindset. However, with NoSQL, it's easier
> to fall into that hole.

Because of the Document storage pattern, it is easy to just throw everything related to the entity
that you are working with into one document. You will tend to design the API (or method, interface,...)
which accepts just one single object containing everything, even the sub entities. As your
application grows, you will add more and more option flags to the API to support for multiple
different cases. When you finally look back, it has become a whole mess. We ran into this problem
before. Initially, we created an API and a user interface for the user to insert and edit the
business entity. It sent the object to the server through a very simple API like this

```
# insert
POST /api/customers
Request Body: Customer object

# update
PATCH /api/customers/:customerId
Request body: Customer object
```

We then introduced the ability to work with its child entities, which are also stored inside the
Customer object itself. The API didn't change at all. We just included all other things inside
that single Customer object (because the query had to start from the parent object anyway).

```json
{
  "name": "Truong",
  "age": 20,
  // other props

  "policies": [
    {
      "number": "sample-number",
      "premium": 200,
      // other props
    }
  ]
}
```

Finally, after years, the API has become a complicated one with many conflicting options.
-  Instead of making multiple simple APIs, we built one multi-purpose API to support many different
use cases (and trust me, nobody can do it in an efficient way).
- Instead of hiding the actual implementation of the backend, we let the same document flow through
from the database to the frontend and back.
- Instead of abstracting and designing things in a loose-coupling fashion, we made everything tight
together in an over-engineering way.

Another example is about the strictness of the schema. Usually, we just throw anything that we want
into the document object. The array prop can contain
multiple items with different schema. The database will never complain about this.
This leads to many edge cases later while developing the application. It is hard to write the
good and reusable code for those items. The codebase will eventually end up with a lot of specific
`if`/`else` and hard to extend.

And many other problems...

Again, this is not a database problem. This is the developer issue. The NoSQL databases just allows
you to make those mistakes easier.

# To be continued

[Part 3]({%post_url 2021-05-09-mistakes-of-software-developer-favor-nosql-over-sql-part-3%})
