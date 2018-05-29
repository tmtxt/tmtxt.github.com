---
layout: post
title: "Merge Sort Summary"
description: ""
categories: [algorithm]
thumbnail: 
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Although
> this was already taught in the University, it's still good to summarise here

# Merge Sort

- Divide array into two halves.
- Sort each half.
  - For each half, continue dividing into 2 halves and do merge sort.
- Merge two halves.

Here is the sample implementation from the Coursera course. Actually, I'm more familiar with the
`while` version.

```java
public class Merge {
    private static void merge(Comparable[] a, Comparable[] aux, int lo, int mid, int hi) {
        for (int k = lo; k <= hi; k++)
            aux[k] = a[k];

        int i = lo, j = mid + 1;
        for (int k = lo; k <= hi; k++) {
            if (i > mid) a[k] = aux[j++];
            else if (j > hi) a[k] = aux[i++];
            else if (less(aux[j], aux[i])) a[k] = aux[j++];
            else a[k] = aux[i++];
        }
    }

    private static void sort(Comparable[] a, Comparable[] aux, int lo, int hi) {
        if (hi <= lo) return;
        int mid = lo + (hi - lo) / 2;
        sort(a, aux, lo, mid);
        sort(a, aux, mid + 1, hi);
        merge(a, aux, lo, mid, hi);
    }

    public static void sort(Comparable[] a) {
        aux = new Comparable[a.length];
        sort(a, aux, 0, a.length - 1);
    }
}
```

<!-- more -->

Running time estimates:
- Assume that Laptop executes 10<sup>8</sup> compares/second.
- Assume that Supercomputer executes 10<sup>12</sup> compares/second.
- Insertion sort: **N<sup>2</sup>**
- Merge sort **N logN**

||insertionsort|insertionsort|insertionsort|mergesort|mergesort|mergesort|
|---|---|
||thousand|million|billion|thousand|million|billion|
|**laptop**|instant|2.8 hours|317 years|instant|1 second|18 min|
|**super**|instant|1 second|1 week|instant|instant|instant|
{: .table }

# Bottom-up Merge Sort

- The reversed way of top-down merge sort
- Pass through array, merging subarrays of size 1.
- Repeat for subarrays of size 2, 4, 8, 16, ....

```java
public class MergeBU
{
    private static void merge(...) {
        // the same
    }

    public static void sort(Comparable[] a)
    {
        int N = a.length;
        Comparable[] aux = new Comparable[N];
        for (int sz = 1; sz < N; sz = sz+sz)
            for (int lo = 0; lo < N-sz; lo += sz+sz)
                merge(a, aux, lo, lo+sz-1, Math.min(lo+sz+sz-1, N-1));
    }
}
```

- Bottom-up merge sort is about 10% slower than recursive, top-down mergesort on typical systems
- Recursive mergesort requires `O(logN)` space for the recursion stack
- The bottom-up version lets you do better (no recursion stack, just a few integers keeping track of
  your position in the input)
- If you come across some language that doesn't support recursion and provides you with only limited
  memory for a stack (perhaps an embedded system?), the bottom-up version will be your only choice.
- [https://stackoverflow.com/a/17902960/3071084](https://stackoverflow.com/a/17902960/3071084)
