---
layout: post
title: "Solutions to 3-sum problem"
description: ""
categories: [algorithm]
tags: [algorithm]
thumbnail: /files/2018-05-05-solution-to-3-sum-problem/img1.png
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course.

# 1. The 3-sum problem

![3-sum not 3-some](/files/2018-05-05-solution-to-3-sum-problem/img1.png)
For fun picture: 3-sum not 3-some 🤣

But I wish there were an algorithm like that in reality 😂

The 3-sum problem is described as below

> Given N distinct integers, how many triples sum to exactly zero?

# 2. Brute-force - N<sup>3</sup> solution

```java
for (int i = 0; i < N; i++)
    for (int j = i+1; j < N; j++)
        for (int k = j+1; k < N; k++)
            if (a[i] + a[j] + a[k] == 0)
                count++;
```

Do **NOT** use this

# 3. N<sup>2</sup>logN solution

* Sort the input array N
* For each pair of numbers `N[i]` and `N[j]`, binary search for the value `-(N[i] + N[j])`
* If exist, count that combination of 3 numbers as 1 3-sum

**Complexity**

|Get pair of number|Binary Search|Total|
|N<sup>2</sup>|logN|N<sup>2</sup>logN|
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

  return count;
}
```

# 4. N<sup>2</sup> solution

- Sort the input array N
- For each item in the array N
  - Try to find 2 item in the array such that `N[i] + N[j] == -x`
  - Initialize `count = 0`
  - Loop from the beginning (`i`) and end (`j`) of the array, for each loop until `i == j`
    - Compute the `sum` of `N[i] + N[j]`
    - If `sum > -x` => `j--` => continue the loop
    - If `sum < -x` => `i++` => continue the loop
    - If `sum == -x` => `count++`, either increase `i` or decrease `j`, continue the loop

**Complexity**

Only 1 loop inside another loop, the total complexity is N<sup>2</sup>

**Pseudo code**

```js
function threeSum(N) {
  let count = 0;

  for(let i = 0; i < N.length; i++) {
    let x = N[i];
    let minusX = 0 - x;

    let i = 0, j = N - 1;
    while (i !== j) {
      const sum = N[i] + N[j];

      if (sum === minusX) {
        count++;
        i++;
      } else if (sum > minusX) {
        j--;
      } else {
        i++;
      }

    }
  }

  return count;
}
```
