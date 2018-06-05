---
layout: post
title: "Binary Heap & Heapsort Summary - Part 1"
description: ""
categories: [algorithm]
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Probably
> this was taught in the University but I don't remember anything, I have no idea about its
> definition and applications until I take this course.

# Binary Heaps

## Heap-ordered Binary Tree

![Heap-ordered Binary Tree](/files/2018-06-05-binary-heap-heapsort-summary-part-1/binary-heap-1.png)

- Each node represents a key
- Parent's key is not smaller than children's keys

## Array Representation

![Array Representation](/files/2018-06-05-binary-heap-heapsort-summary-part-1/binary-heap-2.png)

- Indices start at 1.
- Take nodes in level order.
- No explicit links needed!
- Largest key is a[1], which is root of binary tree
- Can use array indices to move through tree
  - Parent of node at k is at k/2
  - Children of node at k are at 2k and 2k+1

## Promotion in a heap

- **Scenario**: Child's key becomes larger key than its parent's key.
- To eliminate the violation:
  - Exchange key in child with key in parent.
  - Repeat until heap order restored.
- In the below image, the 5th item `T` is not in the correct order
  - `T` is larger than `P` (its parent), exchange
  - `T` is still larger than `S` (its parent), exchange
  - Finally, `T` is in the correct order

![Promotion](/files/2018-06-05-binary-heap-heapsort-summary-part-1/binary-heap-3.png)

```java
private void swim(int k)
{
    while (k > 1 && less(k/2, k))
    {
        exch(k, k/2);
        k = k/2;
    }
}
```
