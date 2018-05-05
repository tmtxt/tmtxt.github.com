---
layout: post
title: "Solutions to 3 sum problem"
description: ""
categories: [algorithm]
tags: [algorithm]
thumbnail: /files/2018-04-30-union-find-summary/img1.png
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course.

# The 3-sum problem

For fun picture: 3-sum not 3-some 🤣

But I wish there were an algorithm like that in reality 😂

![3-sum not 3-some](/files/2018-05-05-solution-to-3-sum-problem/img1.png)

> Given N distinct integers, how many triples sum to exactly zero?

# Brute-force - N<sup>3</sup> solution

```java
for (int i = 0; i < N; i++)
    for (int j = i+1; j < N; j++)
        for (int k = j+1; k < N; k++)
            if (a[i] + a[j] + a[k] == 0)
                count++;
```

Do **NOT** use this

# N<sup>2</sup>logN solution

* Sort the input array N
* For each pair of numbers `N[i]` and `N[j]`, binary search for the value `-(N[i] + N[j])`

**Complexity**

|Get pair of number|Binary Search|Total|
|`N<sup>2</sup>`|`logN`|`N<sup>2</sup>logN`|
{: .table }

**Pseudo code**

```js
function binarySearch(N, val) {
  //... detailed implementation goes here
  // complexity 0(logN)
}

function threeSum(N) {
  var count = 0;

  for (i = 0; i < N.length; i++) {
    for (j = i+1; j < N.length; j++) {
      val = 0 - (N[i] + N[j]);
      if (binarySearch(N, val)) {
        count++;
      }
    }
  }
}
```

# N<sup>2</sup> solution

- Sort the input array N
- For each item in the array N
  - Try to find 2 item in the array such that `N[i] + N[j] == -x`
  - Loop from the beginning (`i`) and end (`j`) of the array, for each loop
    - Compute the `sum` of `N[i] + N[j]`
    - If `i == j` => `false`
    - If `sum == x` => `true` (`count++`)
    - If `sum > x` => `j--` => continue the loop
    - If `sum < x` => `i++` => continue the loop

**Explanation**

For each item `x` in `N`, it can appear in the valid
sum or not. If it appear in the valid triple sum, all the combination which contain `(x, x1, x2)`
will be counted as 1 triple sum => The maximum number of triple sums are N.

**Complexity**

Only 1 loop inside another loop, the total complexity is `N<sup>2</sup>`

**Pseudo code**

```js

```
