---
layout: post
title: "Union Find Summary - Dynamic Connectivity - Union Find"
description: ""
categories: []
tags: []
thumbnail:
---

# 1. Dynamic Connectivity

## The problem

Given a data structure organised as a set of N objects

- Union command: connect 2 objects.
- Find/connected command: is there a path connecting 2 objects?

![](/files/2018-04-30-union-find-summary/img1.png)

{% highlight java %}
union(4, 3);
union(3, 8);
union(6, 5);
union(9, 4);
union(2, 1);

connected(0, 7) return false;
connected(8, 9) return true;

union(5, 0);
union(7, 2);
union(6, 1);
union(1, 0);

connected(0, 7) return true;
{% endhighlight %}

Can only answer the question with `Yes` or `No`. The `Dynamic Connectivity` implementation cannot
answer the exact path between 2 objects. It can only answer whether there are any paths connecting 2
objects.

<!-- more -->

## IsConnected Relation

`IsConnected` relation has these properties

- Reflexive: p is connected to p.
- Symmetric: if p is connected to q, then q is connected to p.
- Transitive: if p is connected to q and q is connected to r, then p is connected to r.

## Connected Components

Maximal set of objects that are mutually connected

![](/files/2018-04-30-union-find-summary/img2.png)

3 connected components

- { 0 }
- { 1 4 5 }
- { 2 3 6 7 }

# 2. Union Find

The Union Find data type contains these 2 main methods

|`UF(int N)`|initialize union-find data structure with N objects (0 to N – 1)|
|`void union(int p, int q)`|add connection between p and q|
|`boolean connected(int p, int q)`|are p and q in the same component?|
{: .table }
