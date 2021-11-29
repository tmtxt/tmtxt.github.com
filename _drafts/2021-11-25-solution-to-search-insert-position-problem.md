---
layout: post
title: "Solution to Search Insert Position problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [Search Insert Position](https://leetcode.com/problems/search-insert-position/)

Given a sorted array of distinct integers and a target value, return the index if the target is
found. If not, return the index where it would be if it were inserted in order.

You must write an algorithm with `O(log n)` runtime complexity.

Example 1
```
Input: nums = [1,3,5,6], target = 5
Output: 2
```

Example 2
```
Input: nums = [1,3,5,6], target = 2
Output: 1
```

Example 3
```
Input: nums = [1,3,5,6], target = 7
Output: 4
```

Example 4
```
Input: nums = [1,3,5,6], target = 0
Output: 0
```

Example 5
```
Input: nums = [1], target = 0
Output: 0
```

Constraints
```
1 <= nums.length <= 104
-104 <= nums[i] <= 104
nums contains distinct values sorted in ascending order.
-104 <= target <= 104
```

<!-- more -->

Simply apply Binary Search to find the item, too simple.

Working code in C#

```csharp
public class Solution {
    public int SearchInsert(int[] nums, int target) {
        return SearchInsert(nums, target, 0, nums.Length - 1, 0);
    }

    public int SearchInsert(int[] nums, int target, int start, int end, int pos) {
        if (start > end) {
            return pos;
        }

        var mid = start + (end - start) / 2;
        var midNum = nums[mid];

        if (midNum == target)
            return mid;

        if (target < midNum)
            return SearchInsert(nums, target, start, mid - 1, mid);

        return SearchInsert(nums, target, mid + 1, end, mid + 1);
    }
}
```
