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
in any Computer Science and Information Technology courses. I will start by briefly reviewing the
fundamental knowledge of Merge Sort. The basic idea behind Merge sort is a divide-and-conquer
strategy, where you break the array into smaller halves, sort each half and then merge them back
together. The recursive process continues until each half contains only 1 item (already sorted).

![Merge Sort Explain](/files/2024-10-24-merge-sort-interview/merge-sort.png)

There are 2 variants of Merge sort implementation, using `for` and `while` loop. You can check my
other post about Merge Sort [here]({% post_url 2018-05-23-merge-sort-summary %}), which contains
the `for` version from the algorithm course on Coursera. For the purpose of
interview questions, I'll demonstrate using the `while` version in this post.

As mentioned above, there are 2 essential steps in Merge Sort, the Divide step and the Merge step.
Let's begin with the Divide step

```javascript
// WARNING: This is not working code and not the most efficient way to do
const mergeSort = (arr) => {
  // base case
  if (arr.length === 1)
    return arr;

  const mid = Math.floor(arr.length / 2);
  const firstHalf = mergeSort(arr.slice(0, mid));
  const secondHalf = mergeSort(arr.slice(mid, arr.length));
  return merge(firstHalf, secondHalf);
}
```

and here is how you merge 2 halves (2 sorted arrays)
```javascript
// WARNING: This is not working code and not the most efficient way to do
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

It's time for you to practice some easy Leetcode questions related to Merge Sort before reading the
next part.
- [Merge Sorted Array](https://leetcode.com/problems/merge-sorted-array/description/)
- [Merge Two Sorted Lists](https://leetcode.com/problems/merge-two-sorted-lists/description/)
([solution]({%post_url 2021-12-10-solution-to-merge-two-sorted-lists-problem%}))
- [Squares of a Sorted Array](https://leetcode.com/problems/squares-of-a-sorted-array/description/)
([solution]({%post_url 2022-10-11-solution-for-squares-of-a-sorted-array-problem%}))

# 2. The Merge Function

We will continue by examining the above `merge` function in detail. Here is another way to
visualize it. Think about it as
**Pick the first item from either array based on some criteria, and then repeat until no items left**

![Merge Function](/files/2024-10-24-merge-sort-interview/merge-function.png)

This time, ask yourself these questions
- What if the arrays are replaced with two linked lists?
- What if they are replaced with two queues?
- How about the arrays with unknown size?
- How do you handle datasets that are too large to fit in memory?

These scenarios often appear in interview problems. You can spot them when
you see the input involves two datasets that can be ordered, such as sorted arrays, linked lists
or queues. For more challenging questions, the interviewer might obscure these details, requiring
you to transform the input into an ordered format (usually by sorting).

Let's take a look at some scenarios

## External Sorting

Merge Sort and its variants are useful for doing External Sorting, a technique to
handle massive amounts of data that don't fit in RAM. The most common question related to this
type is to sort a large file (containing 1 million 8-bit integers) on a computer with only
1MB of RAM. The approach remains the same: divide the file into smaller parts that fit into memory,
sort each of them individually, write the sorted parts to another file and finally merge the
files back together.

![Merge 2 files](/files/2024-10-24-merge-sort-interview/merge-file.png)

```javascript
// pseudo code only
let val1 = readLine(file1);
let val2 = readLine(file2);
while (val1 || val2) {
  if (!val1) {
    writeLine(val2, outFile);
    val2 = readLine(file2);
    continue;
  }

  if (!val2) {
    writeLine(val1, outFile);
    val1 = readLine(file1);
    continue;
  }

  if (val1 < val2) {
    writeLine(val1, outFile);
    val1 = readLine(file1);
  } else {
    writeLine(val2, outFile);
    val2 = readLine(file2);
  }
}
```

Read more [Solution to the Sorting with 1MB RAM computer problem]({% post_url 2018-05-05-solutions-to-the-sorting-with-1mb-ram-computer-problem %})

## Intersection and Union

Another interview problem is to find the intersection
or union of two arrays. **Union** is an array containing elements present in either or both of the
arrays, while **intersection** is an array containing only items present in both arrays. In both
cases, all duplicates are removed and the order of the elements in the result may vary depending on
the implementation.

For problems like these, sorting the input arrays beforehand can simplify it a lot. Once sorted,
apply a modified version of the **merge function** to your 2 sorted arrays. For the union, maintain
two pointers starting at the beginning of both arrays, append the smaller one to the result and
advance the pointer for that array. If the two items are equal, append one of them and advance both
pointers.

To help you imagine it better, here's the visualization of each iteration inside the merge function

![Union](/files/2024-10-24-merge-sort-interview/union.png)

```javascript
// pseudo code only
const res = [];
while (arr1.length || arr2.length) {
  // edge where only arr1 or arr2 has items
  if (!arr1.length) {...}
  if (!arr2.length) {...}

  // decide which array to pick item from and then move the point up
  if (arr1[0] < arr2[0]) {
    // pick from arr1
    res.push(arr1.shift());
  } else if (arr1[0] > arr2[0]) {
    // pick from arr2
    res.push(arr2.shift());
  } else {
    // pick from both
    res.push(arr1.shift());
    arr2.shift();
  }
}
```

A similar process can be adapted to compute the intersection, but only equal elements are added to
the result

![Intersection](/files/2024-10-24-merge-sort-interview/intersection.png)

Here are some similar questions to practice
- What happens if the two input arrays contain duplicate elements?
- [Longest Common Prefix](https://leetcode.com/explore/featured/card/top-interview-questions-easy/127/strings/887/)

## Concurrency Control

The application of Union is not limited to a mathematical problem, but also in another form of
concurrency control. Here are some examples and keywords that 

You can use it to combine data from two sources or coordinate 2 different
processes.

![Concurrency Control](/files/2024-10-24-merge-sort-interview/concurrency.png)

Two-Lane Bridge Problem
Question: A bridge has two lanes, one for vehicles coming in and one for vehicles going out. The bridge can only support one vehicle per lane at a time. Vehicles arrive at different times, and each vehicle wants to either enter or exit the bridge.
- If a vehicle wants to enter and another wants to exit at the same time, prioritize the vehicle that arrived first.
- How would you determine the exact time each vehicle can cross the bridge, respecting the order of arrival and the last usage of the bridge?

Train Station Platform
Question:
A train station has a single platform and two queues: one for passengers wanting to **board** the train and one for passengers wanting to **disembark**.
- The train can either be used for boarding or disembarking at any given time. Passengers arrive at different times and are either boarding or leaving.
- The platform can be used for boarding or disembarking, but the direction depends on who arrived first when simultaneous arrivals happen.
- How would you calculate the exact time each passenger can board or disembark the train?

Single Gate Airport Security
Question:
At an airport, there is a single security gate that allows people to either enter or exit. People arrive at the gate at different times and want to either board a flight or leave the airport.
- If two people arrive at the same time with opposite directions (one entering, one exiting), the direction in which the gate was last used will determine who goes first.
- How would you determine when each person passes through the gate?

Library Entry/Exit System
Question:
A library has a single turnstile and two queues: one for people who want to **enter** the library and another for those who want to **exit**.
- Each person arrives at a specific time, and their direction (enter or exit) is known.
- If two people arrive at the same time with different directions, the decision of who goes first depends on the turnstile's last usage.
- Calculate the time each person will be allowed to pass through the turnstile.

Parking Garage Gate
Question:
A parking garage has a single gate that vehicles can use either to **enter** or to **exit**. Each vehicle has a specific arrival time, and its desired direction (enter or exit) is also given.
- If vehicles with opposite directions arrive at the same time, the gate gives priority based on its last usage.
- Determine when each vehicle will pass through the gate, considering these rules.

Single Lane Toll Booth
Question:
A toll booth has a single lane, and vehicles either want to **pay tolls** or **leave**. Vehicles arrive at the booth at different times, and their direction (whether they want to pay or leave) is given.
If two vehicles arrive at the same time, one wishing to pay and the other wishing to leave, the decision of who goes first depends on the toll booth's last usage.
Calculate the time each vehicle will cross the toll booth.

Hospital Entry System
Question:
A hospital has a single entrance, and people arrive either to **enter** the hospital or **leave**. Each person arrives at a given time with a desired direction (enter or exit).
If people arrive at the same time, the direction the entrance was last used will determine who gets to go first.
How would you compute when each person can pass through the entrance?

Amusement Park Entrance
Question:
An amusement park has a single gate, and guests either want to **enter** or **exit**. The entrance operates like a turnstile and can only process one guest at a time.
- If two guests with different directions arrive at the same time, the direction the gate was last used (enter or exit) determines who goes first.
- How would you determine the exact time each guest will pass through the gate?

Stadium Gate Entry/Exit
Question:
A stadium has a single turnstile and two queues: one for people wanting to **enter** the stadium and another for those wanting to **exit**.
- If two people arrive at the same time, the one with the direction corresponding to the last operation of the turnstile will go first.
- How would you calculate when each person passes through the gate?

Convention Center Turnstile
Question:
A convention center has a single turnstile that can be used for people to either **enter** or **exit**. People arrive at the turnstile at different times, and each person is either entering or leaving.
If two people arrive at the same time and their directions are different, the decision of who goes first depends on the previous usage of the turnstile.
Determine the time when each person passes through the turnstile.