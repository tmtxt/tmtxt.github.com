---
layout: post
title: "Mistakes of a Software Engineer - Favor NoSQL over SQL? - Part 2"
description: ""
categories: [misc]
tags: []
thumbnail:
---

> Part 1 [Mistakes of a Software Engineer - Favor NoSQL over SQL - Part 1]({%post_url 2021-05-03-mistakes-of-software-developer-favor-nosql-over-sql-part-1%})

# There is nothing called Schemaless

One argument that people usually make about NoSQL is the flexibility in schema designing. They
are Schemaless databases and very similar to objects in programming language. They dynamic schema is very
powerful and can support several different use cases, not limited like what SQL provides. You can
simply throw your objects into it and do anything you want.

> Is it really true? For me, it's **NOT**.

There is nothing called Schemaless. The schema just moves from the Database level to Application level.
For Document databases, you are actually using implicit data schema model (compare to explicit data
schema in SQL). Instead of letting the database handle the schema validation, you will have to do it 
yourself in Application code.
- Some databases support simple schema validation. However, none of them are as optimal as what 
  SQL provides. You are limited to just some basic data types (string, number, boolean,...) while 
  SQL offers a wide range of different data types for different use case (for example: varchar, nvarchar 
  text,... just for string data). Working with pre-defined data type is always easier and more performant.
- In case you handle the schema by yourself, it may be ok if you use a static-typed programming language
  (like C#). However, if you are developing your application using Nodejs, Ruby, PHP,... (which is quite
  common for Startup companies), you have maintain another layer of schema validation in your application
  because the data is just runtime objects.

Schema migration is also another problem. NoSQL databases encourage you to just throw the objects
into and not care much about the schema. Some databases don't even ask to create a table, simply
just write the data and the table will be created automatically. This leads to the problem with
schema version. When you read/write the data, it is hard to know which version the data is. You will
have to add many other `if` statement just to check for what should have been provided. No matter
how careful you design your application, how strict your coding conventions are, there will be the
case your code works with outdated data schema. For critical part of the business, I'm pretty sure
that you would want everything to be expressed clearly, not implicitly.

# You are encouraged with bad patterns

> Not really a database issue, it's more about developer mindset. However, with NoSQL, it's easier
> to fall into that hole.

Because of the Document storage pattern, it is easy to just throw everything related to the entity
that you are working with into one document. You will tend to design the API (method, interface,...)
which accepts just one single object containing everything, even the sub entities. As your
application grows, you will add more and more option flags to the API to support for multiple
different cases. When you finally look back, it has become a whole mess. we ran into this problem
before. Initially, we designed an API and a user interface for the user to insert and edit the
entity. We simply send the object to the server through a very simple API like this

```
# insert api
POST /api/customers
Request Body: Customer object

# update api
PATCH /api/customers/:customerId
Request body: Customer object
```

We then introduced the ability to work with its sub entities, which are also stored inside the
Customer object itself. The API didn't change at all. We simply include every other things inside
that single Customer object (because you will have to query and start from the parent object
anyway).

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
use cases (and the fact is nobody can build that kind of API in an efficient way).
- Instead of hiding the actual implementation of the backend, we let the same document flow through
from the database to the frontend and back.
- Instead of abstracting and designing things in a loose-coupling fashion, we made everything tight
together in an over-engineering way.

Another example is about the strictness of the schema. Usually, we just throw anything that we want
into the object, especially for the sub entity (because it is so simple). The array prop can contain
multiple items with different schema. This leads to many edge cases later while developing the
application.

Again, this is not a database problem. This is the developer issue. The database just makes it
easier to make these mistakes.

# To be continued

Part 3 is coming...
