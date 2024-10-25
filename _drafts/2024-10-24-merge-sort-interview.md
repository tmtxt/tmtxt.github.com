---
layout: post
title: "Merge Sort and the Interview"
description: ""
categories: [algorithm]
tags: []
---

> It's not just about Sorting!

# Merge Sort Review

... intro here

There are 2 variants of Merge sort implementation, using `for` and `while` loop. You can check my
other post about Merge sort [here]({% post_url 2018-05-23-merge-sort-summary %}), which contains
the `for` version from the algorithm course on Coursera. For the purpose of
interview, I'll talk about the `while` implementation in this post.

**The basic idea behind Merge sort is to merge 2 sorted arrays**. You can do it by dividing the
array into 2 halves, sort each half and then merge them. For each half, you repeat the same steps
until the each half contains only 1 item (already sorted).

![Merge Sort Explain](/files/2024-10-24-merge-sort-interview/merge-sort-1.png)

This is not working code and not the most efficient way, but it's easy to demonstrate the next part
of this post related to interview topic

```javascript
// merge 2 sorted arrays
const merge  = (arr1, arr2) => {
  const res = [];
  while (arr1.length && arr2.length) {
    const item1 = arr1[0];
    const item2 = arr2[0];
    if (item1 < item2) {
      res.push(item1);
      arr1.shift();
    } else {
      res.push(item2);
      arr2.shift();
    }
  }

  if (arr1.length) {
    res.push(...arr1);
  } else {
    res.push(...arr2);
  }

  return res;
}

const mergeSort = (arr) => {
  if (arr.length === 1)
    return arr;
  
  const mid = Math.floor(arr.length / 2);
  const firstHalf = mergeSort(arr.slice(0, mid));
  const secondHalf = mergeSort(arr.slice(mid, arr.length));
  return merge(firstHalf, secondHalf);
}
```

Basic problems on Merge Sort
- [Merge Sorted Array](https://leetcode.com/problems/merge-sorted-array/description/)
- [Merge Two Sorted Lists](https://leetcode.com/problems/merge-two-sorted-lists/description/) and
[solution]({%post_url 2021-12-10-solution-to-merge-two-sorted-lists-problem%})
- [Squares of a Sorted Array](https://leetcode.com/problems/squares-of-a-sorted-array/description/)
and [solution]({%post_url 2022-10-11-solution-for-squares-of-a-sorted-array-problem%})

# Divide and Conquer

Merge Sort and its variants are extremely useful for doing External Sorting, where you need to
handle massive amounts of data that do not fit in the RAM. The most common question related to this
type is to sort a large file (a file containing 1 million 8-bit integers) using a 1Mb RAM computer.

The idea is still the same. You will need to divide the file into smaller part that fit in the
computer memory, sort each of them, write to another file and then merge those files together.

Read more [Solution to the Sorting with 1MB RAM computer problem]({% post_url 2018-05-05-solutions-to-the-sorting-with-1mb-ram-computer-problem %})

# The Merge part