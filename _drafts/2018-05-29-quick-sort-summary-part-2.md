---
layout: post
title: "Quick Sort summary - Part 2"
description: ""
categories: [algorithm]
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Although
> this was already taught in the University, I remember nothing about it because I haven't touched
> it for the long time.

# Selection Problem

> Given an array of N items, find a k<sup>th</sup> smallest item. For example, an array A =
> `[5, 2, 9, 4, 10, 7]`, the 3rd smallest item is `5`, the 2nd smallest item is `4` and the smallest
> item is `2`

**The idea**

- Based on Quick Sort
- Partition the array so that
  - Entry `a[j]` is in place.
  - No larger entry to the left of `j`
  - No smaller entry to the right of `j`
- Repeat in one sub-array, depending on `j`, finished when `j` equals `k`

```java
public static Comparable select(Comparable[] a, int k) {
    StdRandom.shuffle(a);
    int lo = 0, hi = a.length - 1;
    while (hi > lo)
    {
        int j = partition(a, lo, hi);
        if (j < k) lo = j + 1;
        else if (j > k) hi = j - 1;
        else return a[k];
    }
    return a[k];
}
```

The idea is that, after each partition, the item `a[j]` will be moved to the right place. So it will
become the **j<sup>th</sup>** smallest item. Repeat that process until we can find `j = k`.

![Quick Select](/files/2018-05-27-quick-sort-summary/quick-select.png)

**Quick-select** takes **linear** time on average. **Quick-select** uses **~½ N<sup>2</sup>**
compares in the worst case, but (as with quicksort) the random shuffle provides a probabilistic
guarantee.

# Quick Sort - Duplicate Keys

-  Quick Sort goes quadratic unless partitioning stops on equal keys!
-  **~½N<sup>2</sup>** compares when all keys equal.
  - B A A B A B B **B** C C C
  - A A A A A A A A A A **A**
- Solve by using **3-way partitioning**

**3-way partitioning**: partition array into 3 parts so that:
- Entries between **lt** and **gt** equal to partition item **v**
- No larger entries to left of **lt**
- No smaller entries to right of **gt**

![3-way](/files/2018-05-27-quick-sort-summary/3way.png)

- Let `v` be partitioning item `a[lo]`
- Scan `i` from left to right.
  - `(a[i] < v)`: exchange `a[lt]` with `a[i]`; increment both `lt` and `i`
  - `(a[i] > v)`: exchange `a[gt]` with `a[i]`; decrement `gt`
  - `(a[i] == v)`: increment `i`

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
