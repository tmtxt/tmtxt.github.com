---
layout: post
title: "Union Find Summary - Part 3 - Quick Union"
description: ""
categories: [algorithm]
tags: [algorithm]
thumbnail: /files/2018-04-30-union-find-summary/img1.png
---

# Approach

The underlying data structure for `Quick Union` is similar to `Quick Find`. We need an array for all
the items. The values are still the id, but it's the id of the parent item. The connected component
is organised using a tree structure like this

![](/files/2018-04-30-union-find-summary/img5.png)

![](/files/2018-04-30-union-find-summary/img6.png)

- `isConnected(p, q)` check whether the 2 items have the same root
- `union(p, q)` set the id of p's root to the id of q's root

<!-- more -->

![](/files/2018-04-30-union-find-summary/img7.png)

![](/files/2018-04-30-union-find-summary/img8.png)

# Sample implementation

{% highlight java %}
{
  private int[] id;

  public QuickUnionUF(int N)
  {
    // set id of each object to itself
    // (N array accesses)
    id = new int[N];
    for (int i = 0; i < N; i++) id[i] = i;
  }

  private int root(int i)
  {
    // chase parent pointers until reach root
    // (depth of i array accesses)
    while (i != id[i]) i = id[i];
    return i;
  }

  public boolean connected(int p, int q)
  {
    // check if p and q have same root
    // (depth of p and q array accesses)
    return root(p) == root(q);
  }

  public void union(int p, int q)
  {
    // change root of p to point to root of q
    // (depth of p and q array accesses)
    int i = root(p);
    int j = root(q);
    id[i] = j;
  }
}
{% endhighlight %}

# Complexity

|Method|Complexity||
|----|----|----|
|`isConnected(p, q)`|O(N)|Worst case, have to traverse through all items|
|`union(p, q)`|O(n)|Only need to update 1 id but including the cost of finding roots|
{: .table }

=> `Quick Union` is faster than `Quick Find` when performing `isConnected` command but still too slow
for large data set.
