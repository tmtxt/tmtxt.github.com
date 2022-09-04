---
layout: post
title: "Sample code for Binary Search algorithm"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [Binary Search](https://leetcode.com/problems/binary-search/)

This is so trivial. I just put it here so I can look up faster.

Given an array of integers `nums` which is sorted in ascending order, and an integer `target`, write a
function to search `target` in `nums`. If `target` exists, then return its index. Otherwise, return `-1`.
You must write an algorithm with `O(log n)` runtime complexity.

Example 1
```
Input: nums = [-1,0,3,5,9,12], target = 9
Output: 4
Explanation: 9 exists in nums and its index is 4
```

Example 2
```
Input: nums = [-1,0,3,5,9,12], target = 2
Output: -1
Explanation: 2 does not exist in nums so return -1
```

Constraints
```
1 <= nums.length <= 104
-104 < nums[i], target < 104
All the integers in nums are unique.
nums is sorted in ascending order.
```

<!-- more -->

Working code in C#

```csharp
public class Solution {
    public int Search(int[] nums, int target) {
        return Search(nums, target, 0, nums.Length - 1);
    }

    public int Search(int[] nums, int target, int start, int end)
    {
        if (start > end) return -1;

        var mid = (int)((end - start) / 2) + start;
        var midNum = nums[mid];

        if (midNum == target)
            return mid;

        if (target < midNum)
            return Search(nums, target, start, mid - 1);

        return Search(nums, target, mid + 1, end);
    }
}
```
