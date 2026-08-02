---
layout: post
title: "Quick Sort summary - Part 2 - Selection Problem"
description: ""
categories: [algorithm]
mermaid: true
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Although
> this was already taught in the University, I didn't even know that it can be used for Selection
> Problem

- [Part 1 - Basic Implementation]({% post_url 2018-05-29-quick-sort-summary-part-1 %})
- Part 2 - Selection Problems
- [Part 3 - 3-way Partitioning]({% post_url 2018-06-02-quick-sort-summary-part-3 %})

# Selection Problem

Given an array of N items, find a k<sup>th</sup> smallest item. For example, an array A =
`[5, 2, 9, 4, 10, 7]`, the 3rd smallest item is `5`, the 2nd smallest item is `4` and the smallest
item is `2`

# The idea

- Based on Quick Sort
- Partition the array so that
  - Entry `a[j]` is in place.
  - No larger entry to the left of `j`
  - No smaller entry to the right of `j`
- Repeat in one sub-array, depending on `j`, finished when `j` equals `k`

<!-- more -->

# Java Implementation

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

Unlike quick sort, which recurses into **both** halves, quick-select only continues into the
**one** half that can contain the `k`<sup>th</sup> item:

<div class="mermaid">
flowchart TD
    S["partition(a, lo, hi) &rarr; j"] --> D{"compare j with k"}
    D -->|"j &lt; k"| L["kth item is on the right<br/>set lo = j + 1"]
    D -->|"j &gt; k"| R["kth item is on the left<br/>set hi = j - 1"]
    D -->|"j == k"| F["a[k] is in place<br/>return a[k]"]
    L --> S
    R --> S
</div>

Here is an example searching for the item with index `k = 2` (the 3rd smallest). Each row is one
iteration of the loop; notice how the search window `[lo, hi]` keeps shrinking until `j == k`:

| Iteration | lo | hi | partition returns `j` | `j` vs `k` (=2) | Next action |
|-----------|----|----|-----------------------|-----------------|-------------|
| 1 | 0 | 5 | 4 | `j > k` | `hi = j - 1 = 3` |
| 2 | 0 | 3 | 1 | `j < k` | `lo = j + 1 = 2` |
| 3 | 2 | 3 | 2 | `j == k` | return `a[2]` |
{: .table }

# Performance

**Quick-select** takes **linear** time on average. **Quick-select** uses **~½ N<sup>2</sup>**
compares in the worst case, but (as with quicksort) the random shuffle provides a probabilistic
guarantee.
