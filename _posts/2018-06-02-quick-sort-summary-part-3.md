---
layout: post
title: "Quick Sort summary - Part 3 - 3-way Partitioning"
description: ""
categories: [algorithm]
mermaid: true
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

While scanning with pointer `i`, the array is kept in these four regions:

<div class="mermaid">
flowchart LR
    A["&lt; v<br/>a[lo .. lt-1]"] --- B["= v<br/>a[lt .. i-1]"] --- C["not yet seen<br/>a[i .. gt]"] --- D["&gt; v<br/>a[gt+1 .. hi]"]
</div>

| Region | Range | Contents |
|--------|-------|----------|
| Less than `v` | `a[lo .. lt-1]` | entries `< v` |
| Equal to `v` | `a[lt .. i-1]` | entries `== v` |
| Not yet examined | `a[i .. gt]` | still unknown |
| Greater than `v` | `a[gt+1 .. hi]` | entries `> v` |
{: .table }

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

Here is a full trace on the array `[2, 1, 3, 2, 3, 1, 2]`, using `v = a[lo] = 2` as the pivot. The
pointers start at `lt = 0`, `gt = 6`, `i = 0`, and the scan stops as soon as `i > gt`:

| Step | i | lt | gt | `a[i]` vs `v` | Action | Array |
|------|---|----|----|---------------|--------|-------|
| 0 | 0 | 0 | 6 | `2 == v` | `i++` | 2 1 3 2 3 1 2 |
| 1 | 1 | 0 | 6 | `1 < v` | exch `a[lt]`,`a[i]`; `lt++`, `i++` | 1 2 3 2 3 1 2 |
| 2 | 2 | 1 | 6 | `3 > v` | exch `a[i]`,`a[gt]`; `gt--` | 1 2 2 2 3 1 3 |
| 3 | 2 | 1 | 5 | `2 == v` | `i++` | 1 2 2 2 3 1 3 |
| 4 | 3 | 1 | 5 | `2 == v` | `i++` | 1 2 2 2 3 1 3 |
| 5 | 4 | 1 | 5 | `3 > v` | exch `a[i]`,`a[gt]`; `gt--` | 1 2 2 2 1 3 3 |
| 6 | 4 | 1 | 4 | `1 < v` | exch `a[lt]`,`a[i]`; `lt++`, `i++` | 1 1 2 2 2 3 3 |
| 7 | 5 | 2 | 4 | `i > gt` | stop | 1 1 2 2 2 3 3 |
{: .table }

After the scan, the array is split into `[1, 1]` (less than `v`), `[2, 2, 2]` (equal to `v`, already
in place) and `[3, 3]` (greater than `v`). Quick sort then only needs to recurse on the two outer
parts - `a[lo .. lt-1]` and `a[gt+1 .. hi]` - while the equal-keys band in the middle is skipped.

Quick Sort with 3-way partitioning is even shorter (and harder to imagine). I gave one compliment to
the brain that could thought of those above solutions.
