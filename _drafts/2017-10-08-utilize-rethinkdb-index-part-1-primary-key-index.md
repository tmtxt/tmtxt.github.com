---
layout: post
title: "Utilize RethinkDB Index - Part 1 - Primary key Index"
description: ""
categories: [rethinkdb]
tags: [rethinkdb]
thumbnail: files/2017-10-08-utilize-rethinkdb-index-part-1-primary-key-index/thumbnail.png
---

It has been more than one year since my last post. But yeah, I'm still here, not going anywhere.
This time, I write about the database that I have been working over the last one year
at [Agency Revolution](https://www.agencyrevolution.com/), **RethinkDB**.

At **Agency Revolution**, we make heavy use of **RethinkDB**. Nearly everything is stored in
RethinkDB. Probably at the time you are reading this blog post, that will not be true anymore and we
have been utilizing other databases as well. However, as it's still one of our main data storage, we
used to have a lot of performance issues related to storing and retrieving data (and we still have
until now). This blog post is to summarize how we use RethinkDB indexes to solve those problems as
well as some use cases for different kind of indexes in RethinkDB.

Before you continue reading this post, there are something that I have say

- For me, RethinkDB is dead now. It's not well-maintained and updated for now. However, it's still
one of our main data storage and it's not easy to migrate to another database system without
breaking any existing feature. The business has to keep moving and if the existing feature is still
working well, don't try to break and rebuild it.
- I hate the idea of relying all on a NoSQL database. Each database has its own strengths/weaknesses
and we should use them correctly depending on the requirements.
- I love the databases/programming languages that enforce everything strictly (JavaScript snippets in
this post are for demonstrating purpose only). It seems a bit
annoying for initial development but can save you a lot of time when the product grows. IMO, NoSQL
should be the secondary data storage, acting as a caching/supporting layer for the important data
stored in SQL. I wrote this post not to encourage the heavy use of RethinkDB, but rather to show how
we deal with the scaling problem that we got.
- In the future, after working at Agency Revolution, more likely that I will never touch
RethinkDB again. However, the knowledge about the underlying RethinkDB index will undoubtedly be
helpful when working with other database system.

# RethinkDB Primary Key Index

The first part of this post is about the **Primary Key Index** in RethinkDB. You may be surprised
that I mention Primary Key in a post about indexes
but yeah, Primary Keys are always indexed and it's worth mentioning it as the very first thing that
you should try to take advantage of. It's the simplest index solution that you have for free
for all tables. There are no overhead for using Primary Key Index because it's the default for all
tables. If your record can be identified by a combination of some of its props, don't let RethinkDB
generate the primary key. Build the primary id yourself and use that to query when needed.

Take the email marketing system for example, in a microservice system, you would probably build a
microservice for acting as a gateway for sending out all your emails. That microservice needs to
keep track of all the sent emails to avoid re-sending the same email multiple times. Each email
request sent to this gateway includes these information

- `emailTemplateId`: the id of the template used to render the email content
- `emailContent`: the content of the email
- `emailMetaData`: meta data
- `customerId`: the id of the receiving customer
- `emailAddress`: the recipient email address

The simplest solution would be to create a compound index (will be discussed later) on that table,
which includes the necessary fields for finding the correct email in the `sentEmails` table. For
each request, the microservice has to perform a query to the `sentEmails` table to check whether the
email already exists or not. After that, it has to write back the sent email information to that
table when finish sending.

However, the question that the query needs to answer is just whether email exists or not. If there
is a solution to identify the sent email (the primary key), maintaining a secondary index beside
the primary key would be a waste of database resource and can slow down write performance since it
has to update the secondary index, too.

To eliminate the need for a secondary index, simply build the sent email id
ourselves. Each email object can be identified using this combination

- `emailTemplateId`
- `customerId`
- `emailAddress`

{% highlight js %}
function buildEmailId(email) {
  const emailTemplateId = email.emailTemplateId;
  const customerId = email.customerId;
  const emailAddress = email.toEmail;
  return emailTemplateId + customerId + emailAddress;
}
{% endhighlight %}

And the flow to send one email

{% highlight js %}
// check whether the email exists or not
function* checkEmailExists(email) {
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

// send email
function* sendEmail(email) {
  const emailExists = yield checkEmailExists(email);
  if (emailExists) {
    return;
  }

  yield sendEmail(email);
  yield storeSentEmail(email);
}
{% endhighlight %}

# RethinkDB Primary Index in Table joining

Another case where this type of index would be helpful is when you want to separate less frequently
used data into another table.

Let's say you are building an integration module between your email marketing system and many other
third party AMS systems. Your system needs to connect and download the customers data from those AMS
system, map those customers into your customer schema and persist them into the database.
You have to store some meta data to each mapped customer. The meta data also contains
the original data from that AMS system for reference, which is usually quite large but rarely used
(because you will mostly work with the standardized data in your schema). Storing everything in one
customer record is probably not a good idea since every query need to operate on the whole big
customer object, but we rarely need to access the meta data.

The solution is to separate the meta data into another table and only join that data when necessary.
The join will operation will be performed on the primary key of the meta data table.

Back to the example, after you receive the customer object from the 3rd party AMS system and map it
to your schema, here is the flow how you would store that record in the database

{% highlight js %}
function buildMetaDataKey(mappedCustomer) {
  // build something unique here, for example
  return account.thirdPartyId;
}

function* insertCustomer(mappedCustomer) {
  const metaData = mappedCustomer.metaData;
  const customer = _.omit(mappedCustomer, 'metaData');

  // if your customer has some props to identify it, building the primary key
  // for meta data here will let you run 2 insert commands in parallel.
  // otherwise, you would need to insert the meta data first, get back the
  // generated id and set to the customer object
  const metaDataKey = buildMetaDataKey(customer);
  customer.metaDataKey = metaDataKey;
  // use the metaDataKey as the id for the meta data for joining later
  metaData.id = metaDataKey;

  yield [
    r.table('customers').insert(customer),
    r.table('customerMetaData').insert(metaData)
  ];
}
{% endhighlight %}

To query the customer by id

{% highlight js %}
function* getCustomerById(customerId, includeMetaData) {
  let query = r.table('customers').get(customerId);

  // join using the metaDataKey prop in the customer
  if (includeMetaData) {
    query = query.eqJoin('metaDataKey', r.table('customerMetaData')).zip();
  }

  return yield query
}
{% endhighlight %}

Querying multiple customers is similar

{% highlight js %}
function* filterCustomers(filterProps) {
  let query = r.table('customers').filter(filterProps);

  // join using the metaDataKey prop in the customer
  if (includeMetaData) {
    query = query.eqJoin('metaDataKey', r.table('customerMetaData')).zip();
  }

  return yield query;
}
{% endhighlight %}

# To be continue...

The first part of this series does not really show you how to create extra indexes for faster
querying but rather show you how to avoid creating unnecessary indexes and take advantage of the
existing indexes to speed up queries. The next part will be about working with secondary indexes,
how to use them correctly and efficiently.
