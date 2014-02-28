---
layout: post
title: "Tree structure query with PostgreSQL"
description: ""
category: misc
tags: [postgres]
---
{% include JB/setup %}

# Introduction

When dealing with hierarchical data, we usually use recursion for constructing
the tree structure. It would be a performance problem if you query the tree node
data from the database every time the recursion happens. Also, if you work with
languages which are heavily based on callback like Javascript, you will find it
is very difficult to deal with the recursion because you have to determine when
the callback has finished execution. Luckily, PostgreSQL supports recursive
query using the `WITH RECURSIVE` keyword. All the data structure can be returns
within a single query.

# WITH queries in PostgreSQL

According to PostgreSQL document, `WITH` queries provide a method for dealing
with large queries by creating temporary tables that exist only for that query.
This is a very simple example from Postgres document to illustrate the usage of
`WITH` queries.

<!-- more -->

{% highlight sql %}
WITH regional_sales AS (
        SELECT region, SUM(amount) AS total_sales
        FROM orders
        GROUP BY region
     ), top_regions AS (
        SELECT region
        FROM regional_sales
        WHERE total_sales > (SELECT SUM(total_sales)/10 FROM regional_sales)
     )
SELECT region,
       product,
       SUM(quantity) AS product_units,
       SUM(amount) AS product_sales
FROM orders
WHERE region IN (SELECT region FROM top_regions)
GROUP BY region, product;
{% endhighlight %}

In the above example, the `WITH` clause creates 2 temporary tables
regional_sales and top_region that exist only for the last `SELECT`
command.

# WITH RECURSIVE queries in PostgreSQL

`WITH` can be followed by `RECURSIVE` modifier for the query to refer to its
own. This simple query will list all number from 1 to 100 using `WITH RECURSIVE`

{% highlight sql %}
WITH RECURSIVE t(n) AS  -- create a temp table named t
(
    SELECT 1     -- non-recursive term
  UNION ALL
    SELECT n+1 FROM t    -- recursive term
)
SELECT n FROM t LIMIT 100;
{% endhighlight %}

Let me explain a bit about the example. `WITH RECURSIVE t(n)` creates a
temporary tables named **t** with one column **n**. A `WITH RECURSIVE` query
usually includes 2 parts: non-recursive term and recursive term. All the two
`SELECT` query should have exactly the same column as the **t** table.
PostgreSQL will first evaluate the non-recursive term (including discarding all
duplicate rows if you're using just UNION not UNION ALL), put the result into a
temporary table and then evaluate the recursive term (which refers to itself)
and then append the result into the previous temporary table until it reaches
the stop condition or continues forever if there is no stop condition like the
above example. We need to use `LIMIT` in the final `SELECT` command to filter
the result.

# WITH RECURSIVE for querying tree

Recursion is extremely useful for dealing with hierarchical data structure.
Consider the pedigree tree example, I have 2 tables. One table stores all the
family members information and the other table contains all the relations in the
pedigree (i.e. the parent-children relation). The **PedigreeRelations** tables
references to **People** table.

![Alt Text](/files/2014-02-28-tree-structure-query-with-postgresql/tables.png)

Here is the query to create these two tables

{% highlight sql %}
CREATE TABLE "People"
(
  id serial NOT NULL,
  name character varying NOT NULL,
  CONSTRAINT pk_people PRIMARY KEY (id)
)

CREATE TABLE "PedigreeRelations"
(
  "parentId" integer NOT NULL,
  "childId" integer NOT NULL,
  CONSTRAINT pk_pedigree PRIMARY KEY ("parentId", "childId"),
  CONSTRAINT fk_pedigree_child FOREIGN KEY ("childId")
      REFERENCES "People" (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT fk_pedigree_inside FOREIGN KEY ("parentId")
      REFERENCES "People" (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
{% endhighlight %}

Insert some sample data

{% highlight sql %}
INSERT INTO "People" ("name") VALUES
('Root husband'),('F1.1 inside'),('F1.2 inside'),('F1.3 inside'),
('F2.1 inside'),('F2.2 inside'),('F2.3 inside'),('F2.4 inside'),('F3.1 inside')

INSERT INTO "PedigreeRelations" ("parentId","childId") VALUES
(1,2),(1,3),(1,4),(2,5),(2,6),(2,7),(3,8),(7,9)
{% endhighlight %}

Now we come to the most chalenging part, querying the descendants with a given
id. I have modified the search_graph query from
[PostfreSQL doc](http://www.postgresql.org/docs/9.1/static/queries-with.html) a
bit to fit my data in 2 tables. Here is how the query look like

{% highlight sql %}
WITH RECURSIVE nodes(parentId, parentName, childId, childName, path, depth) AS (
	SELECT
		r."parentId", p1."name",
		r."childId", p2."name",
		ARRAY[r."parentId"], 1
	FROM "PedigreeRelations" AS r, "People" AS p1, "People" AS p2
	WHERE r."parentId" = 1 -- change this to the root id
	AND p1."id" = r."parentId" AND p2."id" = r."childId"
	UNION ALL
	SELECT
		r."parentId", p1."name",
		r."childId", p2."name",
		path || r."parentId", nd.depth + 1
	FROM "PedigreeRelations" AS r, "People" AS p1, "People" AS p2,
		nodes AS nd
	WHERE r."parentId" = nd.childId
	AND p1."id" = r."parentId" AND p2."id" = r."childId" -- AND depth < 2
)
SELECT * FROM nodes;
{% endhighlight %}

Remember this query is to find all the descendants of a given id, so you may
need to update the `r."parentId" = 1` in the 7th line to the id that you want to
find.  
The result of this query is all its descendants (not include itself). The
result also contains a column (array type) holding the path from the given node
to the node in the current row and a column carries the depth of the current
node compare to the node we passed in before.  
If you have a huge amount of data
and you just want to query all descendant nodes within a specific depth level,
uncomment `-- AND depth < 2` in the 17th line and change 2 to whatever depth
level that you want.

![Alt Text](/files/2014-02-28-tree-structure-query-with-postgresql/result.png)

