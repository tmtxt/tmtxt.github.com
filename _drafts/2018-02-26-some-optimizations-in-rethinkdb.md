---
layout: post
title: "Some optimizations in RethinkDB"
description: ""
categories: [rethinkdb]
tags: []
thumbnail: 
---

> Yes it’s RethinkDB. Please don’t shout at me why you still write about the optimisation for a discontinued product like RethinkDB. I’m neither a fan of RethinkDB nor NoSQL. It is because I have to work with RethinkDB right now, deal with all the pains of RethinkDB and NoSQL and the team cannot move away from it since there are a lot of services currently depend on RethinkDB. But hey, most of the enhancements that we made are actually the basic philosophy in databse scaling and optimisation. All those theory can be applied later in other database system, not just RethinkDB.

So, you may already know that, at the time of writing this post, I am working at Agency Revolution. We have been running the system which relies on RethinkDB for more than 3 years. We have built a great and highly scalable system with it. Beside that, we also have faced a lot of difficulties when the system grew too quickly, when the number requests peek during real life events (the agency needed to send a lot of emails before holiday or after the disaster) or when large amount of data came in and out of the system. We have applied a lot of solutions in order to cope with the increase of work load so that our RethinkDB clusters can still serve the user within an acceptable time range. Some of those optimisations will be mentioned in this post.

<!-- more -->

# Check existence

There are several ways to check whether an item with a specified primary key exists in a table in RethinkDB. One possible way is to pull the record out using the `get` command

```js
// returns true if exists, otherwise returns false
function* checkExistence(userId) {
	const user = yield r.table(‘users’).get(userId);
	return !!user;
}
```

This works ok. But since you only need to check whether the user exists in the system or not, it’s necessary to get the whole user object, transfer it into the application just to check if it exists or not. One better solution is to compare it directly in RethinkDB

```js
// returns true if exists, otherwise returns false
function* checkExistence(userId) {
	return yield r.table(‘users’).get(userId).eq(null);
}
```

This is better because the is no data transfering between the database server and your application, which can save a large amount of network usage. However, if you do it frequently and the **user** objects are large, this solution puts the workload to the disk. Let’s take a look at RethinkDB query profiler

![not-equal](/files/2018-02-26-some-optimizations-in-rethinkdb/not-equal.png)

As you can see, RethinkDB performs read on disk (in case the document has not been cached in memory) when you compare with `null`. That is not efficient when everything you want is just to check for one primary key exists or not. Luckily, the primary keys are stored on the primary key index, which is usually stored in the database server memory. This query is executed purely on the index tree without any disk read required

```js
function* checkExistence(userId) {
	return yield r.table(‘users’).getAll(userId).count().eq(1);
}
```

Here is the result from the query analyser

![get-all](/files/2018-02-26-some-optimizations-in-rethinkdb/get-all.png)

> Lesson learnt: Most of the time, the primary keys are indexed automatically by the database server (both SQL and NoSQL). For any queries that require only primary key, use the query analyser (QueryProfiler in RethinkDB, `EXPLAIN` query in Postgres,...) to make sure we only query on the index tree. Avoid reading the disk when not necessay.

So what is the use case for the above scenario? Checking the existence, of course! In the concurrent world, some unimportant actions can be scheduled to run asynchronously later, of out peek time. There might be the case where the subject of the action was deleted before that. In that situation, the action should be ignored. For example, you have one timer worker that runs outside of business hour everyday to combine user activities data and calculate a report for each user to store into another database for analysis. If the user was deleted in that day, it should be ignored from that job to reduce the stress for the database. Another use case is to make sure one action will not be performed twice by logging each success action into a database record. I mentioned that before in this post [Utilize RethinkDB primary index]({%post_url 2017-10-08-utilize-rethinkdb-index-part-1-primary-key-index%}).