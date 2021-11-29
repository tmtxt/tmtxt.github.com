---
layout: post
title: "Solution to Find First and Last Position of Element in Sorted Array problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode:

Given an array of integers `nums` sorted in non-decreasing order, find the starting and ending position of a given `target` value.

If `target` is not found in the array, return `[-1, -1]`.

You must write an algorithm with `O(log n)` runtime complexity.

Example 1
```
Input: nums = [5,7,7,8,8,10], target = 8
Output: [3,4]
```

Example 2
```
Input: nums = [5,7,7,8,8,10], target = 6
Output: [-1,-1]
```

Example 3
```
Input: nums = [], target = 0
Output: [-1,-1]
```

Constraints
```
0 <= nums.length <= 105
-109 <= nums[i] <= 109
nums is a non-decreasing array.
-109 <= target <= 109
```

<!-- more -->

Solution: Perform a binary search on the array. When the target value is found, that means the
beginning could be before that position. Set the `high` to that position and continue the binary
search on the range before that to find the starting position. Do the same strategy to find the
ending position.

Sample code in C#
```csharp
public class Solution {
    public int[] SearchRange(int[] nums, int target)
    {
        var start = FindStart(nums, target);
        if (start == -1)
            return new[] { -1, -1 };

        var end = FindEnd(nums, target);
        return new[] { start, end };
    }

    public int FindStart(int[] nums, int target)
    {
        var low = 0;
        var high = nums.Length - 1;

        var result = -1;

        while (low <= high)
        {
            var mid = low + (high - low) / 2;

            if (target < nums[mid])
                high = mid - 1;
            else if (target > nums[mid])
                low = mid + 1;
            else
            {
                high = mid - 1;
                result = mid;
            }

        }

        return result;
    }

    public int FindEnd(int[] nums, int target)
    {
        var low = 0;
        var high = nums.Length - 1;

        var result = -1;

        while (low <= high)
        {
            var mid = low + (high - low) / 2;

            if (target < nums[mid])
                high = mid - 1;
            else if (target > nums[mid])
                low = mid + 1;
            else
            {
                low = mid + 1;
                result = mid;
            }

        }

        return result;
    }
}
```
