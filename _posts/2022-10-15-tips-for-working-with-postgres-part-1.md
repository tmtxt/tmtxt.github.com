---
layout: post
title: "Tips for working with Postgres - Part 1"
description: "Just a collection of tips to make working with Postgres (and other SQL-like databases) easier"
categories: [misc]
thumbnail:
---

> Just a collection of tips to make working with Postgres (and other SQL-like databases) easier

# Measurement Unit

Take this case for an example, you have a product table and you want to store product information like its size
and weight by adding these columns to describe such properties: `width`, `length`, `height` and
`weight`.

| id | name    | width | length | height | weight |
|----|---------|-------|--------|--------|--------|
| 1  | ipad    | 10    | 20     | 0.1    | 0.5    |
| 2  | macbook | 20    | 30     | 0.5    | 2      |
{: .table }

So what’s the problem with the above table? We are assuming that the size props (width, height and
length) are measured in **cm** and the weight is measured in **kg**. Usually, we could put this
logic in application layer to make sure we convert everything to **cm** and **kg** before inserting
into the database. However, it could lead to even more problems

- What will happen if a new dev join the team? How can you make sure that person will know when to
  convert and when not?
- What will happen if a dev using **pound** join the team?
- What will happen if a dev accidentally assume the value in **weight** is in **mg**?
- Sometimes, you could do a double conversion, making thing worse.
- You need to remember adding comment to every place in your code, just to remind people which
  measurement unit that function is using.
- Which data type to choose? Integer, of course, is not a good choice. However, working with real, double,
  decimal or numeric is always harder compare to int. They could cause some problems with parsing
  and datatype for languages/libraries like Nodejs.

<!-- more -->

Solving this issue is actually quite straight forward, you will need to
- Make it explicit: Append the measurement unit suffix to the prop name, for example `widthMm`,
  `widthCm`, `weightKg`, `weightMg`,...
- Use the smallest possible unit to avoid the issue with decimal number, simply use integer
  for everything and do the conversion if necessary on application layer. In the above example, you
  should stick with `widthMm`, `lengthMm`, `heightMm` and `weightMg`.

# Down Migration

Normally, Database migration tools offer 2 modes of migration: **Up** and **Down**. The **Up**
migrations are usually the normal and happy path. The **Down** ones are usually used to undo the
changes of the **Up** migrations if there are anything wrong. However, over time, your **Down**
migrations will become rediculously unpredictable

* How often do you test the **Down** migrations? Usually the answer is *Never*. How confident are
  you with the **Down** migrations? You may fix a problem with an error **Up** migration by
  introducing a worse problem.
* How about the data? What happen if an **Up** migration removes some data? How can the **Down**
  migration recover the data? You can never recover the previous state correctly. Your database will
  end up in an inconsistent state, which is even more terrible. Trust me, Inconsistency is one of
  the most annoying problem when dealing with database.

So the solution is simply to delete all the down migrations from your source control. When you want
to fix a problem, create another **Up** migration to correct your mistake and go through your normal
SDLC testing workflow.
