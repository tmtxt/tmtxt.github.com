---
layout: post
title: "Union Find Summary - Part 5 - Related Interview Questions"
description: ""
categories: [algorithm]
tags: [algorithm]
thumbnail: /files/2018-04-30-union-find-summary/img1.png
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Here are
> some `Quick Union` related interview questions and my answers

- [Part 1 - Union Find]({% post_url 2018-04-30-union-find-summary-part-1 %})
- [Part 2 - Quick Find]({% post_url 2018-04-30-union-find-summary-part-2 %})
- [Part 3 - Quick Union]({% post_url 2018-04-30-union-find-summary-part-3 %})
- [Part 4 - Improved Quick Union]({% post_url 2018-05-01-union-find-summary-part-4 %})
- Part 5 - Related Interview Questions

# 1. Social network connectivity

### Question

Given a social network containing n members and a log file containing m
timestamps at which times pairs of members formed friendships, design an algorithm to determine the
earliest time at which all members are connected (i.e., every member is a friend of a friend of a
friend ... of a friend). Assume that the log file is sorted by timestamp and that friendship is an
equivalence relation. The running time of your algorithm should be mlogn or better and use extra
space proportional to n.

### Answer

The earliest time at which all members are connected is when we union all into 1 connected
component (1 tree). That means all the nodes in the tree have the same root.  
This is an improvement of weighted quick union algorithm. Every time we call the union, we will
check the weight of the tree to see whether it is equal to the size of n.

<!-- more -->

# 2. Union-find with specific canonical element

### Question

Add a method `find()` to the union-find data type so that `find(i)` returns the largest element in
the connected component containing i. The operations `union()`, `connected()` and `find()` should
all take logarithmic time or better.

For example, if one of the connected components is `{1, 2, 6, 9}` then the `find()` method should
return 9 for each of the four elements in the connected components.

### Answer

We need another array for storing the largest number in each connected component. We only need to
store in the root node. Every time we call the union, we will update the largest number of that
connected component. The find method need to traverse to the root node get that largest value. The
complexity is just O(lgN) (equal to the connected method).

# 3. Successor with delete

### Question

Given a set of `n` integers `S={0,1,...,n−1}` and a sequence of requests of the following form:

- Remove x from S
- Find the successor of x: the smallest y in S such that y≥x.

Design a data type so that all operations (except construction) take logarithmic time or better in
the worst case.

### Answer

Not sure whether I understand this correctly. But we can store that in 2 arrays, just like a double
linked list. Every time we remove 1 item, we just need to update the value to point to the previous
and next. The complexity is 1.
