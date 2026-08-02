---
layout: post
title: "Binary Heap and Heapsort Summary - Part 2 - Heapsort"
description: ""
categories: [algorithm]
mermaid: true
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Probably
> this was taught in the University but I don't remember anything, I have no idea about its
> definition and applications until I take this course.
> Part 1 here [Binary Heap & Heapsort Summary - Part 1 - Binary Heap]({% post_url 2018-06-07-binary-heap-heapsort-summary-part-1 %})

# The Idea

Heapsort has two phases: build a max-heap from the array, then repeatedly remove the maximum to
fill the array from the back. We use the array `S O R T E X A M P L E` (`N = 11`) as the example.

<div class="mermaid">
graph LR
    A["Arbitrary array<br/>S O R T E X A M P L E"] --> B["Max-heap<br/>X T S P L R A M O E E"] --> C["Sorted array<br/>A E E L M O P R S T X"]
</div>

- Start with array of keys in arbitrary order

| Index | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 |
|-------|---|---|---|---|---|---|---|---|---|----|----|
| Key   | S | O | R | T | E | X | A | M | P | L  | E  |
{: .table }

- Create a max-heap with all N keys

<div class="mermaid">
graph TD
    q1["X (1)"] --> q2["T (2)"]
    q1 --> q3["S (3)"]
    q2 --> q4["P (4)"]
    q2 --> q5["L (5)"]
    q3 --> q6["R (6)"]
    q3 --> q7["A (7)"]
    q4 --> q8["M (8)"]
    q4 --> q9["O (9)"]
    q5 --> q10["E (10)"]
    q5 --> q11["E (11)"]
</div>

- Repeatedly remove the maximum key (in place) to create a sorted array

| Index | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 |
|-------|---|---|---|---|---|---|---|---|---|----|----|
| Key   | A | E | E | L | M | O | P | R | S | T  | X  |
{: .table }

<!-- more -->

# First step: Heap construction

Build heap using bottom-up method. Start with the lowest nodes and go up each level, use `sink`
operation to correct the heap. Highlighted arrows show the path each sunk node travels.

**Starting point (arbitrary order):**

<div class="mermaid">
graph TD
    p1["S (1)"] --> p2["O (2)"]
    p1 --> p3["R (3)"]
    p2 --> p4["T (4)"]
    p2 --> p5["E (5)"]
    p3 --> p6["X (6)"]
    p3 --> p7["A (7)"]
    p4 --> p8["M (8)"]
    p4 --> p9["P (9)"]
    p5 --> p10["L (10)"]
    p5 --> p11["E (11)"]
</div>

All the nodes in the lowest level (`M`, `P`, `L`, `E` at indices 8-11) are already 1-node binary
heaps, so we start sinking from index `N/2 = 5` and work back to the root.

**`sink(5)` on `E`** - it is smaller than child `L`, so it sinks down to index 10:

<div class="mermaid">
graph TD
    p1["S (1)"] --> p2["O (2)"]
    p1 --> p3["R (3)"]
    p2 --> p4["T (4)"]
    p2 --> p5["L (5)"]:::v
    p3 --> p6["X (6)"]
    p3 --> p7["A (7)"]
    p4 --> p8["M (8)"]
    p4 --> p9["P (9)"]
    p5 --> p10["E (10)"]:::v
    p5 --> p11["E (11)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 8 stroke:#c62828,stroke-width:3px
</div>

**`sink(4)` on `T`** - nothing to do, `T` is already larger than both children:

<div class="mermaid">
graph TD
    p1["S (1)"] --> p2["O (2)"]
    p1 --> p3["R (3)"]
    p2 --> p4["T (4)"]:::v
    p2 --> p5["L (5)"]
    p3 --> p6["X (6)"]
    p3 --> p7["A (7)"]
    p4 --> p8["M (8)"]
    p4 --> p9["P (9)"]
    p5 --> p10["E (10)"]
    p5 --> p11["E (11)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
</div>

**`sink(3)` on `R`** - smaller than child `X`, so it sinks to index 6:

<div class="mermaid">
graph TD
    p1["S (1)"] --> p2["O (2)"]
    p1 --> p3["X (3)"]:::v
    p2 --> p4["T (4)"]
    p2 --> p5["L (5)"]
    p3 --> p6["R (6)"]:::v
    p3 --> p7["A (7)"]
    p4 --> p8["M (8)"]
    p4 --> p9["P (9)"]
    p5 --> p10["E (10)"]
    p5 --> p11["E (11)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 4 stroke:#c62828,stroke-width:3px
</div>

**`sink(2)` on `O`** - it sinks through `T` down to index 9:

<div class="mermaid">
graph TD
    p1["S (1)"] --> p2["T (2)"]:::v
    p1 --> p3["X (3)"]
    p2 --> p4["P (4)"]:::v
    p2 --> p5["L (5)"]
    p3 --> p6["R (6)"]
    p3 --> p7["A (7)"]
    p4 --> p8["M (8)"]
    p4 --> p9["O (9)"]:::v
    p5 --> p10["E (10)"]
    p5 --> p11["E (11)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 2,7 stroke:#c62828,stroke-width:3px
</div>

**`sink(1)` on `S`** - smaller than child `X`, so it sinks to index 3, giving the final max-heap:

<div class="mermaid">
graph TD
    p1["X (1)"] --> p2["T (2)"]
    p1 --> p3["S (3)"]:::v
    p2 --> p4["P (4)"]
    p2 --> p5["L (5)"]
    p3 --> p6["R (6)"]
    p3 --> p7["A (7)"]
    p4 --> p8["M (8)"]
    p4 --> p9["O (9)"]
    p5 --> p10["E (10)"]
    p5 --> p11["E (11)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 1 stroke:#c62828,stroke-width:3px
</div>

We finally transform an arbitrary array into a heap-ordered array.

```java
for (int k = N/2; k >= 1; k--)
 sink(a, k, N);
```

# Second step: Sortdown

In order to transform a heap-ordered array into a sorted array, we will repeatedly remove the
largest item in the heap, one at a time. Refer to part 1 for the idea on how to remove the maximum
item in a heap. The only difference is that after exchanging the max with the last item, we will
keep it in the array instead of completely removing it out.

**Starting point (a heap-ordered array):**

<div class="mermaid">
graph TD
    p1["X (1)"] --> p2["T (2)"]
    p1 --> p3["S (3)"]
    p2 --> p4["P (4)"]
    p2 --> p5["L (5)"]
    p3 --> p6["R (6)"]
    p3 --> p7["A (7)"]
    p4 --> p8["M (8)"]
    p4 --> p9["O (9)"]
    p5 --> p10["E (10)"]
    p5 --> p11["E (11)"]
</div>

**Remove the largest item, one at a time, and park it at the end of the array.** The first
iteration swaps the max `X` with the last leaf, shrinks the heap (so `X` becomes the first sorted
item), then sinks the new root `E` back down into place:

<div class="mermaid">
graph TD
    p1["T (1)"] --> p2["P (2)"]
    p1 --> p3["S (3)"]
    p2 --> p4["O (4)"]
    p2 --> p5["L (5)"]
    p3 --> p6["R (6)"]
    p3 --> p7["A (7)"]
    p4 --> p8["M (8)"]
    p4 --> p9["E (9)"]:::v
    p5 --> p10["E (10)"]
    sorted["X (sorted)"]:::s
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    classDef s fill:#a5d6a7,stroke:#2e7d32,stroke-width:2px
    linkStyle 0,2,7 stroke:#c62828,stroke-width:3px
</div>

Repeating this until the heap is empty leaves the array fully sorted:

| Index | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 |
|-------|---|---|---|---|---|---|---|---|---|----|----|
| Key   | A | E | E | L | M | O | P | R | S | T  | X  |
{: .table }

```java
while (N > 1) {
 swap(a, 1, N--);
 sink(a, 1, N);
}
```

# Java Implementation

```java
public class Heap {
    public static void sort(Comparable[] a) {
        int N = a.length;
        // build the heap
        for (int k = N/2; k >= 1; k--)
            sink(a, k, N);

        // convert heap to sorted array
        while (N > 1)
        {
            exch(a, 1, N);
            sink(a, 1, --N);
        }
    }

    private static void sink(Comparable[] a, int k, int N) {
        // implemented in part 1
    }

    private static boolean less(Comparable[] a, int i, int j) { /* compare */ }

    private static void swap(Comparable[] a, int i, int j) { /* swap */ }
}
```

# Heapsort Characteristics

- `Heapsort` is an In-place sorting algorithm with `N logN` worst-case
- Compare to the other sorting algorithm
  - `Mergesort`: not in-place, linear extra space required.
  - `Quicksort`: in-place, but quadratic time in worst case.
- `Heapsort` is optimal for both time and space, but:
  - Inner loop longer than `Quicksort`'s
  - Makes poor use of cache memory
