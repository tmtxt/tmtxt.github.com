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
