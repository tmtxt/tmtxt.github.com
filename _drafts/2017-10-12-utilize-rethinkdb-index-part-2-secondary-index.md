---
layout: post
title: "Utilize RethinkDB Index - Part 2 - Secondary Index"
description: ""
categories: [rethinkdb]
tags: [rethinkdb]
thumbnail: /files/2017-10-08-utilize-rethinkdb-index-part-1-primary-key-index/thumbnail.png
---

This post is the second part of the first post here.


# Some rules when using RethinkDB Indexes

RethinkDB Indexes, similar to Indexes in other database, are the trade-off between read and write
performance. Therefore, the basic rules for RethinkDB Indexes are similar to other database.

- Don't create index if not necessary.
- Don't create index if the data set is small enough so that you can use the `filter` query.
- Indexes require memory to process, be careful with tables that have a lot of indexes.
- Indexes can slow down write operations significantly.

# RethinkDB Simple Secondary Index

Continue from last post, sometimes, what you need is not simply querying by primary key. More often,
you may want to get all the records belong to one category.
In case the number of records is too big for one simple `filter` command, you will need to create
one secondary index for faster querying.

For example, our automation marketing system at **Agency Revolution** supports our clients to create
many campaigns for targeting different kinds of customers. Each client is stored as one `realm` in
the realm table and each campaign is stored as one record in the `campaign` table with one field
`realmId` to reference back to the realm table. Usually, we
will need to get all the campaigns belong to one realm to display to user or to perform other
complex operations. The simplest solution is to create a simple secondary index on the
field `realmId` of the campaign table.

{% highlight js %}
function* createIndex() {
  yield r.table('campaigns').indexCreate('realmId');
}
{% endhighlight %}

To query the campaigns by realm, simply use RethinkDB `getAll` command

{% highlight js %}
function* getCampaignsByRealmId(realmId) {
  const campaigns = yield r.table('campaigns').getAll(realmId, { index: 'realmId' });
  return campaigns;
}
{% endhighlight %}

Joining 2 tables with the help of secondary indexes is also very fast. In case you
want to get all the campaigns belong to one realm with the realm data, simply do

{% highlight js %}
function* getCampaignsWithRealmName(realmId) {
  return yield r
    .table('campaigns')
    .getAll(realmId, { index: 'realmId' })
    .eqJoin('realmId', r.table('realms'))
    .map(function(row) {
      return row('left').merge({ // left is the campaign data
        realmName: row('right')('name')
      });
    });
}
{% endhighlight %}

# RethinkDB Compound Index

Of course, the above indexes cannot satisfy all your needs. You will probably run into the case
where you want to query records based on multiple props and sort the records after querying.
Compound indexes are the rescuer in those cases. If you design it carefully, you can even use one
compound index to serve different kind of queries.

Back to the above example, if the requirements is not just querying all the campaigns belong to one
realm, but also filtering all the campaigns created this month. In that case, we need to create a
compound index like this

{% highlight js %}
function* createIndex() {
  yield r.table('campaigns').indexCreate(
    'realmId_createdAt',
    [r.row('realmId'), r.row('createdAt')]
  );
}
{% endhighlight %}

and to query all the campaigns belong to one realm and were created this month

{% highlight js %}
function* getCampaignsByRealmIdByTimeRange(realmId, fromTimestamp, toTimestamp) {
  return yield r.table('campaigns')
    .between(
      [realmId, fromTimestamp],
      [realmId, toTimestamp],
      {index: 'realmId_createdAt'}
    );
}
{% endhighlight %}

And hey, you can take advantage of the new index to eliminate the previous one `realmId`. If you
want to query all the campaigns belong to one realm, simply use `between` with all the possible
`createdAt` values

{% highlight js %}
function* getCampaignsByRealmId(realmId) {
  return yield r.table('campaigns')
    .between(
      [realmId, r.minval],
      [realmId, r.maxval],
      {index: 'realmId_createdAt'}
    );
}
{% endhighlight %}

This would be a big win when you only to one compound index to server multiple querying scenario.

# Extending the Compound Index

To extend the above example, if your application requires querying all the campaigns belong to one
realm, but care about the type and the status of those campaigns, the index will look like this

{% highlight js %}
function* createIndex() {
  yield r.table('campaigns').indexCreate(
    'realmId_type_status',
    [r.row('realmId'), r.row('type'), r.row('status')]
  );
}
{% endhighlight %}

The above index can serve several cases, from querying exact value to range querying.

{% highlight js %}
// get all campaigns belong to one realm
function* getCampaignsByRealmId(realmId) {
  return yield r
    .table('campaigns')
    .between([realmId, r.minval, r.minval], [realmId, r.maxval, r.maxval], {
      index: 'realmId_type_status'
    });
}

// get all campaigns of one type that belong to one realm
function* getCampaignsByType(realmId, type) {
  return yield r.table('campaigns').between([realmId, type, r.minval], [realmId, type, r.maxval], {
    index: 'realmId_type_status'
  });
}

// get all campaigns of one type with one specific status that belong to one realm
function* getCampaignsByTypeAndStatus(realmId, type, status) {
  return yield r.table().getAll([realmId, type, status], {
    index: 'realmId_type_status'
  });
}
{% endhighlight %}

You can even order the results retrieved from that index

{% highlight js %}
// get all campaigns belong to one realm
// order by type first and then status
function* getCampaignsByRealmId(realmId) {
  return yield r
    .table('campaigns')
    .between([realmId, r.minval, r.minval], [realmId, r.maxval, r.maxval], {
      index: 'realmId_type_status'
    })
    .orderBy({
      index: 'realmId_type_status'
    });
}

// get all campaigns of one type that belong to one realm
// order by the status
function* getCampaignsByType(realmId, type) {
  return yield r
    .table('campaigns')
    .between([realmId, type, r.minval], [realmId, type, r.maxval], {
      index: 'realmId_type_status'
    })
    .orderBy({
      index: 'realmId_type_status'
    });
}
{% endhighlight %}

# A Note on Compound Index

## Use one index at a time

Yeah, you can only use one index in your query. Look at the above example, you can only use one
index `realmId_type_status` for filtering and sorting. Attempting to use different indexes in the
chaining query will result in failure. As far as I understand, the `getAll`/`between` commands will
narrow down the search to one sub-tree on the binary tree index and then sort on that sub-tree only.
That's why you cannot use different indexes in one chaining query.

## The order of the fields is important

If your need is just to query by the exact value of the compound index, the order of the fields is
not important. For example, you can change the above index to whatever order (`type_realmId_status`,
`status_type_realmId`) and this query will always produce the same result with the same speed

{% highlight js %}
// get all campaigns of one type with one specific status that belong to one realm
function* getCampaignsByTypeAndStatus(realmId, type, status) {
  return yield r.table().getAll([status, type, realmId], {
    index: 'status_type_realmId'
  });
}
{% endhighlight %}

However, when it comes to range querying, it's another story. If you take a closer look at the above
queries, you will notice that the left-most values are always fixed/determined values while the
right-most values can be vary. If you want to do a range query with the vary left-most
values, it will similar to a whole table sequence scan.

For example, to get all the campaigns belong to one
realm, this will work efficiently

{% highlight js %}
function* getCampaignsByRealmId(realmId) {
  return yield r
    .table('campaigns')
    .between(
      [realmId, r.minval],
      [realmId, r.maxval],
      {index: 'realmId_type'}
    );
}
{% endhighlight %}

This one also works but it's similar to a sequence scan and is not efficient

{% highlight js %}
function* getCampaignsByRealmId(realmId) {
  return yield r
    .table('campaigns')
    .between(
      [r.minval, realmId],
      [r.maxval, realmId],
      {index: 'type_realmId'}
    );
}
{% endhighlight %}
