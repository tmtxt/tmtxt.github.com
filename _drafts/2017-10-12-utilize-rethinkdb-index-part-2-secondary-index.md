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
you may want to get all the records belong to one specific category.
In case the number of records is too big for one simple `filter` command, you will need to create
one secondary index for faster querying.

For example, you are organizing all the products in your warehouse into several categories. Each
category is stored as one record in the `categories` table as below

{% highlight json %}
{
  "id":             "category id",
  "categoryName":   "category name"
}
{% endhighlight %}

Each product is stored as one record in the `products` table with one prop `categoryId` for
referencing back to the `category` table.

{% highlight json %}
{
  "id":             "product id",
  "productName":    "product name",
  "categoryId":     "category id",
  "categoryId":     "0-10",
  "categoryId":     "product status"
}
{% endhighlight %}

If you want to query all products belong to one category, the simplest solution is to create a
simple secondary index on the field `categoryId` of the product table

{% highlight js %}
function* createIndex() {
  yield r.table('products').indexCreate('categoryId');
}
{% endhighlight %}

To query all the products by category, simply use RethinkDB `getAll` command

{% highlight js %}
function* getProductsByCategoryId(categoryId) {
  return yield r.table('products').getAll(categoryId, { index: 'categoryId' });
}
{% endhighlight %}

Joining 2 tables with the help of secondary indexes is also very fast. In case you
want to get all the products belong to one category with the category name, simply do

{% highlight js %}
function* getProductsWithCategoryName(categoryId) {
  return yield r
    .table('products')
    .getAll(categoryId, { index: 'categoryId' })
    .eqJoin('categoryId', r.table('categories'))
    .zip();
}
{% endhighlight %}

Or if you want to do the reverse way, find the category by id and all its related products

{% highlight js %}
function* getCategoryByIdAndProducts(categoryId) {
  return yield r.table('categories')
    .get(categoryId)
    .merge({
      products: r.table('products')
        .getAll(categoryId, {index: 'categoryId'})
        .coerceTo('array')
    });
}
{% endhighlight %}

# RethinkDB Compound Index

As your application grows bigger, your requirements may be extended to filtering products based on
more properties, not just `categoryId`. That is where a compound
index would be useful. If you design it carefully, you can even use one compound index to serve many
different kinds of queries.

Back to the above example, if we change the requirements to find all the products belong to one
category and has the rating is 5, the index would look like this

{% highlight js %}
function* createIndex() {
  yield r.table('products').indexCreate(
    'categoryId_rating',
    [r.row('categoryId'), r.row('rating')]
  );
}
{% endhighlight %}

and the `getAll` query will receive an array param like this

{% highlight js %}
function* getProductByCategoryIdAndRating(categoryId, rating) {
  return yield r.table('products').getAll([categoryId, rating], { index: 'categoryId_rating' });
}
{% endhighlight %}

A compound index is also helpful when you want to do a range query, too. If you want to find all the
products belong to one category with rating from 1 to 5, use `between` in combination with that
index

{% highlight js %}
function* getProductsByCategoryInRatingRange(categoryId, fromRating, toRating) {
  return yield r.table('products')
    .between(
      [categoryId, fromRating],
      [categoryId, toRating],
      { index: 'categoryId_rating' }
    );
}

getProductsByCategoryInRatingRange('sample-cat-id', 1, 5);
{% endhighlight %}

And hey, you can also take advantage of the new index to eliminate the previous one `categoryId`. If
you want to search for all the products belong to one category, simply use `between` with all the
possible `rating` values

{% highlight js %}
function* getProductsByCategoryId(categoryId) {
  return yield r.table('products')
    .between(
      [categoryId, r.minval],
      [categoryId, r.maxval],
      { index: 'categoryId_rating' }
    );
}
{% endhighlight %}

This would be a big win when you only to one compound index to serve multiple purposes.

# Extending the Compound Index

To extend the above example, if your application requires querying all the products belong to one
category, but also care about the rating and status of those products, the index will look like this

{% highlight js %}
function* createIndex() {
  yield r.table('products').indexCreate(
    'categoryId_rating_status',
    [r.row('categoryId'), r.row('rating'), r.row('status')]
  );
}
{% endhighlight %}

The above index be used for several use cases, from exact value to range querying.

- Get all products belong to one category

{% highlight js %}
function* getProductsByCategoryId(categoryId) {
  return yield r
    .table('products')
    .between([realmId, r.minval, r.minval], [realmId, r.maxval, r.maxval], {
      index: 'categoryId_rating_status'
    });
}
{% endhighlight %}

- Get all products belong to one category, which have specific rating score

{% highlight js %}
function* getProductsByCategoryAndRating(categoryId, rating) {
  return yield r
    .table('products')
    .between([categoryId, rating, r.minval], [categoryId, rating, r.maxval], {
      index: 'categoryId_rating_status'
    });
}
{% endhighlight %}

- Get all products by 3 props

{% highlight js %}
function* getProducts(categoryId, rating, status) {
  return yield r.table().getAll([categoryId, rating, status], {
    index: 'categoryId_rating_status'
  });
}
{% endhighlight %}

You can even order the results retrieved from that index

- Get all products belong to one category, order by rating first and then status

{% highlight js %}
function* getProductsByCategoryId(categoryId) {
  return yield r
    .table('products')
    .between([realmId, r.minval, r.minval], [realmId, r.maxval, r.maxval], {
      index: 'categoryId_rating_status'
    })
    .orderBy({
      index: 'categoryId_rating_status'
    });
}
{% endhighlight %}

- Get all products belong to one category, with a specific rating score, order by the status

{% highlight js %}
function* getProductsByCategoryAndRating(categoryId, rating) {
  return yield r
    .table('products')
    .between([categoryId, rating, r.minval], [categoryId, rating, r.maxval], {
      index: 'categoryId_rating_status'
    })
    .orderBy({
      index: 'categoryId_rating_status'
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
