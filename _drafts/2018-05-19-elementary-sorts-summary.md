---
layout: post
title: "Elementary Sorts Summary"
description: ""
categories: [algorithm]
thumbnail:
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Although
> this was already taught in the University, it's still good to summarise here

# 1. Selection Sort

* In iteration `i`, find index min of smallest remaining entry.
* Swap `a[i]` and `a[min]`

![Selection Sort Gif](/files/2018-05-19-elementary-sorts-summary/selection-sort.gif)

```java
class Selection {
    public static void sort(Comparable[] a) {
        int N = a.length;
        for (int i = 0; i < N; i++) {
            int min = i;
            for (int j = i + 1; j < N; j++)
                if (less(a[j], a[min]))
                    min = j;
            swap(a, i, min); // swap the 2 items
        }
    }
}
```

=> `O(n^2)`
