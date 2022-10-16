---
layout: post
title: "Tips when working with Postgres - Part 2"
description: "Just a collection of tips to make working with Postgres (and other SQL-like databases) easier"
categories: [misc]
thumbnail:
---

> Just a collection of tips to make working with Postgres (and other SQL-like databases) easier

# Integration data

Usually when you build a system that integrates with other 3rd party service, you will need to store
integration information related to the entity, for example the id of the entity on the 3rd party
system or some of its configuration on that system. Imagine that you are building an e-commerce
related product, you may want to sync the Sales order information from Shopify to do the analytics
on customer behavior. The first solution you can think of is to add a column like
*shopify_entity_id* on that table.

- What will happens if you introduce another integration later? Does the name *shopify_entity_id*
  still make sense? You may consider renaming it to *external_entity_id*. How do you know where it
  comes from? Adding another *source* column? How do you store extra 3rd party information about the
  sales order? Keep adding columns like *external_something*? Do those columns actually belong to
  the *sales_order* table itself?
- What will happen if an single entity exists on multiple 3rd party system? For instance, the sales
  order may be presented on both Shopify and on another Shipping service. How would you deal with
  it? Keep adding more columns? What if we introduct another integration?
  - A Json (Jsonb) column could solve the above issue but also creates a whole new problem. How
      about schema enforcement and constraint? How do we make sure that nobody will accidentally
      update it the an incorrect schema? How about `null` and `undefined` values (in case you are
      working with Javascript)? How about indexing the values for quick access? You can index inside
      the json but it just makes things more complicated due to those schema problems mentioned
      above.

The solution, of course, is a SQL approach: make an entity integration table
(*sales_order_integration* in this case). It’s a 1-N relationship, 1 sales order could have 0 or
multiple integrations

*sales_order* table

| id | shipping_address | price | weightMg |
|----|------------------|-------|----------|
| 1  | Ho Chi Minh city | 10    | 20       |
| 2  | Hanoi            | 20    | 30       |
{: .table }

*sales_order_integration* table

| id | sales_order_id | external_entity_id | source      |
|----|----------------|--------------------|-------------|
| 1  | 1              | external-id1       | SHOPIFY     |
| 2  | 1              | external-id2       | WOOCOMMERCE |
| 3  | 2              | external-id3       | SHOPIFY     |
{: .table }

<!-- more -->

You may add another *integration* table (and sub-tables if needed) as a generalization to store
other information like the access/refresh token, for example

| id | client_id | source      | access_token |
|----|-----------|-------------|--------------|
| 1  | 1         | SHOPIFY     | xxx-xxx-xxx  |
| 2  | 1         | WOOCOMMERCE | yyy-yyy-yyy  |
| 3  | 2         | SHOPIFY     | zzz-zzz-zzz  |
{: .table }

and then the *sales_order_integration* table would become

| id | sales_order_id | external_entity_id | integration_id |
|----|----------------|--------------------|----------------|
| 1  | 1              | external-id1       | 1              |
| 2  | 1              | external-id2       | 2              |
| 3  | 2              | external-id3       | 3              |
{: .table }

Here are some query scenarios
- Find *sales_order* by id: of course, you always have index on primary key
- Find external ids by *sales_order_id*: simply add an index on *sales_order_id* column of the
  *sales_order_integration* table and you’are good to go.
  - You could use this when you want to refecth new data of the entity from a specific 3rd party system
  - You could also use this when you want to write updated data to the other systems
- Find *sales_order_id* by *external_entity_id*: you can add an index on *external_entity_id*
  column. Another option is a unique constraint on *integration_id, external_entity_id* (which makes
  more sense since each entity in your system should correspond to only one entity in the other
  system). A unique constraint also acts as an index. A simple JOIN like this will utilize both the
  unique index and the primary key index on 2 tables
  ```sql
  SELECT *
  FROM sales_order s INNER JOIN sales_order_integration i
  ON s.id = i.sales_order_id
  WHERE i.integration_id = $1 AND i.external_entity_id = $2
  ```
  - This is usually seen in webhook handler, where you receive the update about each individual
    sales order. You can use this method to quickly look up the sales order using its external id.
