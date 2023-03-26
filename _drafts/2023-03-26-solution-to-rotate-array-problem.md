---
layout: post
title: "Solution to Rotate Array problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode [Rotate Array](https://leetcode.com/problems/rotate-array/)

Given an array, rotate the array to the right by `k` steps, where `k` is non-negative.

**Example 1**:
```
Input: nums = [1,2,3,4,5,6,7], k = 3
Output: [5,6,7,1,2,3,4]
Explanation:
rotate 1 steps to the right: [7,1,2,3,4,5,6]
rotate 2 steps to the right: [6,7,1,2,3,4,5]
rotate 3 steps to the right: [5,6,7,1,2,3,4]
```

**Example 2**:
```
Input: nums = [-1,-100,3,99], k = 2
Output: [3,99,-1,-100]
Explanation:
rotate 1 steps to the right: [99,-1,-100,3]
rotate 2 steps to the right: [3,99,-1,-100]
```

**Constraints**:
```
- 1 <= nums.length <= 105
- -231 <= nums[i] <= 231 - 1
- 0 <= k <= 105
```

<!-- more -->

**Solution**

Here are the working code in C#

Solution 1: Time `O(n)`, Space `O(n)`
```csharp
public class Solution {
    public void Rotate(int[] nums, int k)
    {
        var res = new int[nums.Length];
        for (int i = 0; i < nums.Length; i++)
        {
            var newPos = (i + k) % nums.Length;
            res[newPos] = nums[i];
        }

        for (int i = 0; i < res.Length; i++)
        {
            nums[i] = res[i];
        }
    }
}
```

Solution 2: Time `O(n)`, Space `O(1)`. Set `k=k%n`, reverse the whole array, then reverse
the array from `0` to `k-1` and then from `k` to `n-1`
```csharp
public class Solution
{
    public void Rotate(int[] nums, int k)
    {
        k %= nums.Length;
        Reverse(nums, 0, nums.Length - 1);
        Reverse(nums, 0, k - 1);
        Reverse(nums, k, nums.Length - 1);
    }

    public void Reverse(int[] nums, int start, int end)
    {
        while (start < end)
        {
            (nums[start], nums[end]) = (nums[end], nums[start]);
            start++;
            end--;
        }
    }
}
```
