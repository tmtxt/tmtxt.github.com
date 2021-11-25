---
layout: post
title: "Sample code for some Binary Search exercises"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

Here are the sample code for some basic Binary Search exercises written in C#. These solutions are
so trivial so I won't explain, just include the sample code here so I can look up faster.

# Basic Binary Search

Given an array of integers nums which is sorted in ascending order, and an integer target, write a
function to search target in nums. If target exists, then return its index. Otherwise, return `-1`.
You must write an algorithm with O(log n) runtime complexity.

- Example 1: Input: `nums = [-1,0,3,5,9,12]`, `target = 9`, Output: `4`, Explanation: `9` exists in nums and its index is `4`
- Example 2: Input: `nums = [-1,0,3,5,9,12]`, `target = 2`, Output: `-1`, Explanation: `2` does not exist in nums so return `-1`

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

<!-- more -->

# First Bad Version

You are a product manager and currently leading a team to develop a new product. Unfortunately, the
latest version of your product fails the quality check. Since each version is developed based on the
previous version, all the versions after a bad version are also bad.

Suppose you have n versions `[1, 2, ..., n]` and you want to find out the first bad one, which
causes all the following ones to be bad.

You are given an API `bool isBadVersion(version)` which returns whether version is bad. Implement a
function to find the first bad version. You should minimize the number of calls to the API.

Examples
```
Example 1:
Input: n = 5, bad = 4
Output: 4
Explanation:
call isBadVersion(3) -> false
call isBadVersion(5) -> true
call isBadVersion(4) -> true
Then 4 is the first bad version.

Example 2:
Input: n = 1, bad = 1
Output: 1
```

Code
```csharp
/* The isBadVersion API is defined in the parent class VersionControl.
      bool IsBadVersion(int version); */
public class Solution : VersionControl
{
    public int FirstBadVersion(int n)
    {
        int left = 1;
        int right = n;

        while (left < right)
        {
            int mid = left + (right - left) / 2;
            if (IsBadVersion(mid))
            {
                right = mid;
            }
            else
            {
                left = mid + 1;
            }
        }

        return left;
    }
}
```

# Search Insert Position

Given a sorted array of distinct integers and a target value, return the index if the target is
found. If not, return the index where it would be if it were inserted in order.

You must write an algorithm with `O(log n)` runtime complexity.

Examples
```
Example 1:
Input: nums = [1,3,5,6], target = 5
Output: 2

Example 2:
Input: nums = [1,3,5,6], target = 2
Output: 1

Example 3:
Input: nums = [1,3,5,6], target = 7
Output: 4

Example 4:
Input: nums = [1,3,5,6], target = 0
Output: 0

Example 5:
Input: nums = [1], target = 0
Output: 0
```

Constraints
- `1 <= nums.length <= 104`
- `-104 <= nums[i] <= 104`
- `nums` contains **distinct** values sorted in **ascending** order.
- `-104 <= target <= 104`

Code
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
