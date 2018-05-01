---
layout: post
title: "Union Find Summary - Part 4 - Improved Quick Union"
description: ""
categories: [algorithm]
tags: [algorithm]
thumbnail: /files/2018-04-30-union-find-summary/img1.png
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course.

# Weighted Quick Union

- Use `Quick Union` but avoid tall tree, to avoid traversing through very long path
- Use an extra array to track the size of each tree (stored in the root node)
- Link the smaller tree to the root of the larger tree
- The size of the new tree is the total size of both tree

![](/files/2018-04-30-union-find-summary/img9.jpg)

<!-- more -->

|Method|Complexity||
|----|----|----|
|`isConnected(p, q)`|O(lg`N`)|Depth of any node x is at most lg`N`|
|`union(p, q)`|O(lg`N`)||
{: .table }

> `lg` = base-2 logarithm

# Quick Union with Path Compression

- Use `Quick Union` but try to flatten the tree
- Every time we compute the root of one item (by traversing through the path to the root), set that
  item to point directly to the root.
- Next time when we call the function to find the root of that same item, we don't have to traverse
  through the full path again

![](/files/2018-04-30-union-find-summary/img10.jpg)

![](/files/2018-04-30-union-find-summary/img11.jpg)

For path compression, if the `root` is called many times enough, the complexity will become `O(1)`
when the tree become flatten.
