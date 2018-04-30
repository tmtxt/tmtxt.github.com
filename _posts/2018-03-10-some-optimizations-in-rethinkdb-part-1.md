---
layout: post
title: "Some optimizations in RethinkDB - Part 1"
description: ""
categories: [rethinkdb]
tags: [rethinkdb]
thumbnail: /files/2017-10-08-utilize-rethinkdb-index-part-1-primary-key-index/thumbnail.png
---

> Yes it’s RethinkDB. Please don’t shout at me why you still write about the optimisations for a discontinued product like RethinkDB. I’m neither a fan of RethinkDB nor NoSQL. It is because I have to work with RethinkDB right now, deal with all the pains of RethinkDB and NoSQL and the team cannot move away from it since there are a lot of services currently depend on RethinkDB. But hey, most of the enhancements that we made are actually the basic philosophy in databse scaling and optimisation. All those theory can be applied later in other database systems, not just RethinkDB.

So, you may have already known that, at the time of writing this post, I am working at Agency Revolution. We have been running the system which relies on RethinkDB for more than 3 years. We have built a great and highly scalable system with it. Beside that, we also have faced a lot of difficulties when the system grew too quickly, when the number requests peek during real life events (the agencies needed to send a lot of emails before holiday or after the disaster) or when large amount of data came in and out of the system. We have applied a lot of solutions in order to cope with the increase of work load so that our RethinkDB clusters can still serve the user within an acceptable time range. Some of those optimisations will be mentioned in this post.

<!-- more -->

# Check existence

There are several ways to check whether an item with a specified primary key exists in a table in RethinkDB. One possible way is to pull the record out using the `get` command

```js
// returns true if exists, otherwise returns false
function* checkExistence(userId) {
	const user = yield r.table('users').get(userId);
	return !!user;
}
```

This works ok. But since you only need to check whether the user exists in the system or not, it’s necessary to get the whole user object, transfer it into the application just to check if it exists or not. One better solution is to compare it directly in RethinkDB

```js
// returns true if exists, otherwise returns false
function* checkExistence(userId) {
	return yield r.table('users').get(userId).eq(null);
}
```

This is better because there is no data transfering between the database server and your application, which can save a large amount of network usage. However, if you do it frequently and the **user** objects are large, this solution puts the workload to the disk. Let’s take a look at RethinkDB query profiler

![not-equal](/files/2018-02-26-some-optimizations-in-rethinkdb/not-equal.png)

As you can see, RethinkDB performs read on disk (in case the document has not been cached in memory) when you compare with `null`. That is not efficient when everything you want is just to check for one primary key exists or not. Luckily, the primary keys are stored on the primary key index, which is usually stored in the database server memory. This query is executed purely on the index tree without any disk read required

```js
function* checkExistence(userId) {
	return yield r.table('users').getAll(userId).count().eq(1);
}
```

Here is the result from the query analyser

![get-all](/files/2018-02-26-some-optimizations-in-rethinkdb/get-all.png)

> Lesson learned: Most of the time, the primary keys are indexed automatically by the database server (both SQL and NoSQL). For any queries that require only primary key, use the query analyser (`QueryProfiler` in RethinkDB, `EXPLAIN` query in Postgres,...) to make sure we only query on the index tree. Avoid reading the disk when not necessay.

So what is the use case for the above scenario? Checking the existence, of course! In the concurrent world, some unimportant actions can be scheduled to run asynchronously later, of out peek time. There might be the case where the subject of the action was deleted before that. In that situation, the action should be ignored. For example, you have one timer worker that runs outside of business hour everyday to combine user activities data and calculate a report for each user to store into another database for analysis. If the user was deleted in that day, it should be ignored from that job to reduce the stress for the database. Another use case is to make sure one action will not be performed twice by logging each success action into a database record. I mentioned that before in this post [Utilize RethinkDB primary index]({% post_url 2017-10-08-utilize-rethinkdb-index-part-1-primary-key-index %}).

# Working with large documents

This is one of the most annoying things when working with NoSQL and is one of the main reasons that push me to the SQL side. I don't want to start a flame war between NoSQL and SQL but there are a lot of issues when dealing with large documents in NoSQL and one of my best advice is to stay away from it if possible. However, life is not that easy! Many applications stick with NoSQL from the beginning (like the application that I'm working at AR) so we still have to deal with that issue.

So, what are the problems when working with large objects? RethinkDB (and other NoSQL databases) discourages normalization and table joining. Typically, when working with NoSQL, you are encouraged to store everything related to one entity into one document to avoid reading from multiple tables, eliminating table joining to increase reading speed (because all the related data blocks are stored near each other on disk). That's fine for the  case where the documents are relatively small. However, as your application and business grow to larger size, more and more data are required to stored into the each individual document. And this will make you feel regret for choosing a NoSQL database before.

Let's take a look at an example in the insurance industry (because I'm working for an insurance related company 😋). Each customer record usually contains all the policies related to that customer. At first, you may see this approach has one benefit that there are no joining required for getting the customer and all its related data. You don't need to care whether you need to put one more index on the `customerId` field on the `policies` table. Everything you need to do is just one query to get the record by id like this

```js
r.table('customers').get(customerId);
```

In the beginning, each customer contains only 1-2 policies. As time goes by, the customer continues to buy more policies. You application also support more features that need to store a lot more data into the customer record. However, most of the time you don't really need all the customer data. If you want to check whether a customer is valid for sending marketing email, you may only need some fields like `email` to check for valid email address and`status` to check for active customer. Although most of the NoSQL databases offer a query to pluck only the necessary fields to return to the client, for example

```js
r.table('customers')
  .get(customerId)
  .pluck(['email', 'status']);
```

This simple solution can help you reduce the network data transfering. However, there are no mechanisms for it to know which exact blocks of data on disk it need to read. What it does is to read the whole document into memory, pick the necessary fields and return to the application. This puts the stress on disk IO if you constantly read from a table containing a lot of large documents just for 1-2 fields. SQL databases force you to define the data type and the size for most columns (int, float, varchar(20) for instance) and can specify exactly where it needs to read on the disk.

One possible workaround for this to create a copy table of the original one. The cloned table contains only some frequently used fields to avoid stressing disk IO. You are free to do any read-related operations on this table because all the records are quite small now so it can benefit from the power of NoSQL. There are some cases where the records size are small enough, we may read the whole table at once and do some complex logic in application level, which cannot be handled by just database queries. However, ensuring the synchronisation between 2 tables can be challenging. You need to care about all the changes in the main table and then reflect the changes to the cloned table (if that change contains any data related to the fields used in the cloned table). We usually have to prepare all the necessary scripts so to remove and recreate all the cloned tables from the main ones in case of error.

According to this post [Data modeling in RethinkDB](https://rethinkdb.com/docs/data-modeling/), if the array properties inside the document (in this case the policies inside the customer object) are quite large, updating one policy record may require reading the whole document to memory, change one field and then write back the entire data into disk. The post also suggests another solution is to split the document into multi tables and perform joining on those tables to get the necessary data. We also applied this streategy, sometimes directly in the database query, sometimes in the application level if it is complicated. However, that sounds not right 😅 NoSQL is not designed that way. There are a lot of complex joining queries that you can not do with NoSQL but tend to be quite easy with SQL. With SQL, you can avoid many cases that you have to load the documents into the application to do some complex processing before joining.

Another strategy is to use another caching layer for only the necessary data that you need. If you often use just some fields from those records, storing only those fields into Redis can also we a good choice. Redis provides a TTL mechanism to clear the records automatically. The flow for retrieving one record is to look it up in Redis first, if it is not found, perform a RethinkDB query, store it to Redis and then return the result to user. This one is similar to the the first solution that you create another cloned table but in another database. This method helps increase the speed of our application a lot. There is another benefit for using this. If the data is not very critical (can be updated a bit late), there are no need for syncing between the 2 data source because the data will be clear after the TTL expires.

# Summary

Above are some streategies that I and the team at AR applied to optimise the performance of RethinkDB. For me, those ideas are helpful for not only RethinkDB but also other NoSQL databases. It helped me a lot in designing the data structure as well as the database decision that I will make in the future. One of my best experience is to stay away from it if possible. Only deal with it when you have to.

> TO BE CONTINUED...
