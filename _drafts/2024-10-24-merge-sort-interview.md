---
layout: post
title: "Merge Sort and the Interview"
description: ""
categories: [algorithm]
tags: []
thumbnail: /files/2024-10-24-merge-sort-interview/merge-sort.png
---

> It's not just about Sorting!

# 1. Merge Sort

Merge Sort is one of the most commonly taught sorting algorithm (beside Quick Sort)
in any Computer Science and Information Technology courses. I will start by briefly review the
fundamentals of Merge Sort. The basic idea behind Merge sort is a divide-and-conquer strategy,
where we break the array into smaller halves, sort each half and then merge them back together.
The recursive process continues until each half contains only 1 item (already sorted).

There are 2 variants of Merge sort implementation, using `for` and `while` loop. You can check my
other post about Merge sort [here]({% post_url 2018-05-23-merge-sort-summary %}), which contains
the `for` version from the algorithm course on Coursera. For the purpose of
interview questions, I'll demonstrate using the `while` version in this post, which is more suitable
for datasets with undetermined number of item.

![Merge Sort Explain](/files/2024-10-24-merge-sort-interview/merge-sort.png)

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
```

```javascript
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

# 2. The Merge Function

Let's take a look at the above Merge function. Its applications go beyond the concept of sorting.
The idea can be used to solve a wide variety of problem, like external sorting, intersection and
union or even some form of concurrency control.

During your interview, you may be able to spot this type of solution when you see the input contains
two datasets that can be ordered, could be 2 sorted arrays, 2 linked lists or 2 queues. For more
advanced questions, they may also involve transforming the input data to the ordered sets (usually
by sorting).

![Merge Function](/files/2024-10-24-merge-sort-interview/merge-function.png)

## External Sorting

Merge Sort and its variants are extremely useful for doing External Sorting, where you need to
handle massive amounts of data that do not fit in the RAM. The most common question related to this
type is to sort a large file (a file containing 1 million 8-bit integers) using a 1Mb RAM computer.

The idea is still the same. You will need to divide the file into smaller part that fit in the
computer memory, sort each of them, write to another file and then merge those files together.

Read more [Solution to the Sorting with 1MB RAM computer problem]({% post_url 2018-05-05-solutions-to-the-sorting-with-1mb-ram-computer-problem %})

## Intersection and Union

Another problem that people are usually asked to do during an interview is to find the Intersection
or Union of 2 sorted arrays (and also unsorted arrays as long as you can convert them to sorted).

https://www.geeksforgeeks.org/longest-common-prefix-using-sorting/
https://leetcode.com/explore/featured/card/top-interview-questions-easy/127/strings/887/

## Concurrency Control