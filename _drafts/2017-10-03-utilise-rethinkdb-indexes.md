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

Yes, it's the simplest index solution that you have for free for all tables. There are no overhead
for using primary index because it's the default for all tables. If your record can be identified by a
combination of some of its props, don't let RethinkDB generate the primary key.
Build the primary id yourself and use that to query when needed.

For example, in our system, we have a gateway for sending out all emails, we call it
**email microservice**.
Our email microservice needs to keep track of all the sent emails to avoid re-sending the same email
multiple times. Each email request sent to this gateway needs to be logged into
the database so that if the request is retried or in case of code bug, that same
email won't get sent twice. The email object can be identified using this
combination

- `emailTemplateId`: the id of the template to render that email
- `accountId`: the id of the receiving account
- `emailAddress`: the email address of that account (in case that account has many emails)

The simplest solution would be to create a compound index on that table, which
includes the value of those 3 fields.
However, that can cause a huge performance issue if
this microservice has to process a large number of requests in peak time.
For each request, the microservice has to perform a query to the sent emails
table to check whether the email is already exists or not. After that, it has to
write back the sent email information to that table. The database has to
maintain 2 separate indexes and we actually don't take advantage of the existing
primary key index.

To eliminate the need for a second index, we simply just build the sent email id
ourselves, using a combination of those 3 fields. For example

{% highlight js %}
function buildEmailId(email) {
  const emailTemplateId = email.emailTemplateId;
  const accountId = email.accountId;
  const emailAddress = email.toEmail;
  return `${emailTemplateId}-${accountId}-${emailAddress}`;
}

// check whether the email exist
function* emailExist(email) {
  const emailId = buildEmailId(email);
  return yield r.branch(
    // query by primary key index
    r.table('sentEmails').get(emailId),
    true,
    false
  );
}

// insert the sent email
function* storeSentEmail(email) {
  const emailId = buildEmailId(email);
  email.id = emailId;
  yield r.table('sentEmails').insert(email);
}
{% endhighlight %}

# RethinkDB Primary Index in Table joining

Another case where this type of index would be helpful is when you want to separate less frequently
used data into another table. For example, at Agency Revolution, we tried to integrate with many other
AMS systems. Each account in our system reflects one account in the original AMS system. To store
that, we need include some meta data to each account. The meta data also contains the original data
from that AMS system, which is usually quite large but rarely used. Storing everything in one
account record is probably not a good idea since every query need to operate on the whole big
account object, but we rarely need to access the meta data. The solution is to separate the meta
data into another table and only join that data when necessary. For instance

To insert the account with meta data

{% highlight js %}
function buildMetaDataKey(account) {
  // build something unique here, for example
  return `${account.realmId}-${account.thirdPartyId}`;
}

function* insertAccount(account) {
  const metaData = account.metaData;
  account = _.omit(account, 'metaData');

  // if your account has some props to identify it, building the primary key
  // for meta data here will let you run 2 insert commands in parallel.
  // otherwise, you would need to insert the meta data first, get back the
  // generated id and set to the account object
  const metaDataKey = buildMetaDataKey(account);
  account.metaDataKey = metaDataKey;
  metaData.id = metaDataKey;

  yield [
    r.table('accounts').insert(account),
    r.table('accountMetaData').insert(metaData)
  ];
}
{% endhighlight %}

To query the account

{% highlight js %}
function* getAccountById(accountId, includeMetaData) {
  let query = r.table('accounts').get(accountId);

  // join using the metaDataKey prop in the account
  if (includeMetaData) {
    query = query.eqJoin('metaDataKey', r.table('accountMetaData')).zip();
  }

  return yield query
}
{% endhighlight %}

Querying multiple accounts is similar

{% highlight js %}
function* filterAccounts(filterProps) {
  let query = r.table('accounts').filter(filterProps);

  // join using the metaDataKey prop in the account
  if (includeMetaData) {
    query = query.eqJoin('metaDataKey', r.table('accountMetaData')).zip();
  }

  return yield query;
}
{% endhighlight %}

# RethinkDB Simple Secondary Index

Sometimes, what you need is not simply querying by primary key. Because RethinkDB is a NoSQL database,
you would need to create a secondary index for joining tables or querying records belong to
one category. For example, our system support client to create many marketing campaigns for targeting
different kinds of customers. Each client is stored as one `realm` in the realm table and each
campaign is stored as one record in the `campaign` table. More often, we will need to get all the
campaigns belong to one realm. The simplest solution is to create a simple secondary index on the
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
compound index to serve for different kind of queries.

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

To extend this, if your requirement is to query all the campaigns belong to one realm, but care
about the type and the status of those campaigns, the index will look like this

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
`status_type_realmId`). The query will always produce the same result with the same speed

{% highlight js %}
// get all campaigns of one type with one specific status that belong to one realm
function* getCampaignsByTypeAndStatus(realmId, type, status) {
  return yield r.table().getAll([status, type, realmId], {
    index: 'status_type_realmId'
  });
}
{% endhighlight %}

However, when it comes to range querying, it's another story. If you take a closer look the above
queries, you will notice that the left-most values are always the values that are fixed and the
right-most values can be the variable ones.
