---
layout: post
title: "Solition to Find (i->j) sequence by sum problem"
description: ""
categories: [algorithm]
---

So, my friend gave me this exercise and asked me to solve it. Sometimes it's fun to solve algorithm problem... sometimes...

> Given an array A of n integer numbers, find all the (i,j) pairs such that A[i] + A[i+1] + A[i+2] + ... + A[j] = 6. For example, A = [1, 5, 4, -2, 4, 8, 7, 9, 0], these pairs are valid i,j (0,1) and (2,4) because sum(A[0->1])=6 and sum(A[2->4])=6

Of course, the worst solution is to perform n<sup>2</sup> operations. For each number in A, do another loop inside that to find the sub-sequence having the sum = 6.

Surely there should be a better solution. And here is the `O(n)` solution.

<!-- more -->

- Loop through A from 0. For each item
  - Calculate the `sum(A[0->i])`. Remember to use an accumulate variable so that you don't have to loop again each time.
- Now, perform another loop through A, for each item `i`, if there exist an `j` that `sum(A[i->j])=6`, which means `sum(A[0->i]) + 6 = sum(A[0->j])`. You job is to look up a value computed before. To do that, use a map to store the result from previous step for `O(1)` access.

The above solution uses 2 loops but can be considered as `O(n)`. You can optimise it more to use a single loop but the basic idea is still the same.

One thing to notice about this is the **sum**. The suggestion from another friend of mine is that, the continuous sequences of numbers have some special attributes that can help us unlock the key to the problem. One of them is the **sum**, probably some days it will help me with other problems.