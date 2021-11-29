---
layout: post
title: "Solution for Squares of a Sorted Array problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [Squares of Sorted Array](https://leetcode.com/problems/squares-of-a-sorted-array/)

Given an integer array `nums` sorted in **non-decreasing** order, return an array of **the squares of each number** sorted in non-decreasing order.

Example 1
```
Input: nums = [-4,-1,0,3,10]
Output: [0,1,9,16,100]
Explanation: After squaring, the array becomes [16,1,0,9,100].
After sorting, it becomes [0,1,9,16,100].
```

Example 2
```
Input: nums = [-7,-3,2,3,11]
Output: [4,9,9,49,121]
```

Constraints
```
1 <= nums.length <= 104
-104 <= nums[i] <= 104
nums is sorted in non-decreasing order.
```

<!-- more -->

> The most trivial way is to square then sort the array, which is an `O(N logN)` solution. Here is
> the `O(N)` solution

Split the array into 2, one for negative number and one for positive number. Since the 2 arrays are
in sorted order, we can use merge sort while computing the square to merge those 2 arrays.

Working code in C#

```csharp
public class Solution {
    public int[] SortedSquares(int[] nums)
    {
        var l1 = new List<int>(); // l1 to store values < 0
        var l2 = new List<int>(); // l2 to store values >= 0

        // store the values < 0 to l1, the ones >= 0 to l2
        for (int i = 0; i < nums.Length; i++)
        {
            if (nums[i] < 0)
                l1.Add(nums[i]);
            else
                l2.Add(nums[i]);
        }

        // reverse l1 so Abs(l[i]) < Abs(l[i+1])
        l1.Reverse();

        var res = new List<int>();
        var arr1 = l1.ToArray();
        var arr2 = l2.ToArray();
        var p1 = 0; // index in arr1
        var p2 = 0; // index in arr2

        // merge sort the sqr
        while (p1 < arr1.Length || p2 < arr2.Length)
        {
            // which array to pick value from? true: pick from arr1, false: pick from arr2
            var whichArr = p1 != arr1.Length && (p2 == arr2.Length || (Math.Abs(arr1[p1]) < arr2[p2]));

            // compute the square
            var val = whichArr ? arr1[p1] : arr2[p2];
            var sqr = val * val;
            res.Add(sqr);

            // increase the pointer of the corresponding array
            if (whichArr)
                p1++;
            else
                p2++;
        }

        return res.ToArray();
    }
}
```
