---
layout: post
title: "Union Find Summary - Part 2 - Quick Find"
description: ""
categories: [algorithm]
tags: [algorithm]
thumbnail: /files/2018-04-30-union-find-summary/img1.png
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course.

- [Part 1 - Union Find]({% post_url 2018-04-30-union-find-summary-part-1 %})
- Part 2 - Quick Find
- [Part 3 - Quick Union]({% post_url 2018-04-30-union-find-summary-part-3 %})
- [Part 4 - Improved Quick Union]({% post_url 2018-05-01-union-find-summary-part-4 %})
- [Part 5 - Related Interview Questions]({% post_url 2018-05-01-union-find-summary-part-5 %})

# Approach

The `Quick Find` idea is to assign all the items in 1 connected component with 1 same id. `p` and
`q` are connected if they have the same id. Every time we do the `union` command, we update those 2
items with the same id (and also all other items in the connected component).

The set is organised as an array, where the values are the id of that connected component. At first,
all the items have the id the same with its index (connected to itself). After each call to `union`,
we update the id of those 2 items to one of them

![](/files/2018-04-30-union-find-summary/img4.png)

<!-- more -->

To check whether the 2 items are connected, simply check whether they have the same id or not. For
example

- `connected(6, 7)` => `true` because 6 and 7 both have id `1`
- `connected(0, 9)` => `false` because 0 has the id `1` and 9 has the id `8`

# Complexity

|Method|Complexity||
|----|----|----|
|`isConnected(p, q)`|O(1)|Only 1 call to check the id of the item|
|`union(p, q)`|O(n^2)|Have to update id of both items all other items having the same id|
{: .table }

=> `union` is too slow for `Quick Find`.

# Implementation

So easy, no need for sample implementation here. Or did I explain too badly?...
