---
layout: post
title: "Solution to Move Zeros"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

Given an integer array `nums`, move all `0`'s to the end of it while maintaining the relative order of the non-zero elements.

Note: that you must do this in-place without making a copy of the array.

Examples
```
Example 1
Input: nums = [0,1,0,3,12]
Output: [1,3,12,0,0]

Example 2
Input: nums = [0]
Output: [0]
```

Constraints
```
1 <= nums.length <= 104
-231 <= nums[i] <= 231 - 1
```

Solution: Maintain 2 pointers, both starting from the beginning of the array. The first pointer
(`i`) traverses the array. If `nums[i]` is `0`, skip to the next one. Otherwise, move `nums[i]` to
`nums[j]` and increase `j`. At the end, all the non-zero numbers will be shifted to the beginning
of the array.

<!-- more -->

```csharp
public class Solution {
    public void MoveZeroes(int[] nums)
    {
        // shift all non-zero numbers to the beginning of the array
        var i = 0;
        var j = 0;
        for (i = 0; i < nums.Length; i++)
        {
            var current = nums[i];
            if (current != 0)
            {
                nums[j] = nums[i];
                j++;
            }
        }

        // backfill the remaining items with 0
        for (int k = j; k < nums.Length; k++)
        {
            nums[k] = 0;
        }
    }
}
```
