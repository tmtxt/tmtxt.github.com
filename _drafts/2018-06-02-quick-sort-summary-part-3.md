---
layout: post
title: "Quick Sort summary - Part 3 - 3-way Partitioning"
description: ""
categories: [algorithm]
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Although
> this was already taught in the University, I remember nothing about it because I haven't touched
> it for the long time.

- [Part 1 - Basic Implementation]({% post_url 2018-05-29-quick-sort-summary-part-1 %})
- [Part 2 - Selection Problems]({% post_url 2018-06-02-quick-sort-summary-part-2 %})
- Part 3 - 3-way Partitioning

# Quick Sort - Duplicate Keys Problem

-  Quick Sort goes quadratic unless partitioning stops on equal keys!
-  **~½N<sup>2</sup>** compares when all keys equal.
  - B A A B A B B **B** C C C
  - A A A A A A A A A A **A**
- Solve by using **3-way partitioning**

# 3-way Partitioning

Partition array into 3 parts so that:
- Entries between **lt** and **gt** equal to partition item **v**
- No larger entries to left of **lt**
- No smaller entries to right of **gt**

![3-way](/files/2018-05-27-quick-sort-summary/3way.png)

- Let `v` be partitioning item `a[lo]`
- Scan `i` from left to right.
  - `(a[i] < v)`: exchange `a[lt]` with `a[i]`; increment both `lt` and `i`
  - `(a[i] > v)`: exchange `a[gt]` with `a[i]`; decrement `gt`
  - `(a[i] == v)`: increment `i`

<!-- more -->

# Java Implementation

```java
private static void sort(Comparable[] a, int lo, int hi) {
    if (hi <= lo) return;
    int lt = lo, gt = hi;
    Comparable v = a[lo];
    int i = lo;
    while (i <= gt)
    {
        int cmp = a[i].compareTo(v);
        if (cmp < 0) exch(a, lt++, i++);
        else if (cmp > 0) exch(a, i, gt--);
        else i++;
    }
    sort(a, lo, lt - 1);
    sort(a, gt + 1, hi);
}
```

![3-way](/files/2018-05-27-quick-sort-summary/3way2.png)

Quick Sort with 3-way partitioning is even shorter (and harder to imagine). I gave one compliment to
the brain that could thought of those above solutions.
