---
layout: post
title: "Solution to Two Sum with Sorted array problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [Two Sum II - Input Array Is Sorted](https://leetcode.com/problems/two-sum-ii-input-array-is-sorted/)

Given a **1-indexed** array of integers `numbers` that is already **sorted in non-decreasing
order**, find two numbers such that they add up to a specific `target` number. Let these two numbers
be `numbers[index1]` and `numbers[index2]` where `1 <= index1 < index2 <= numbers.length`.

Return the *indices* of the two numbers, `index1` and `index2`, **added by one** *as an integer
array `[index1, index2]` of length 2*.

The tests are generated such that there is **exactly one solution**. You **may not** use the same
element twice.

Example 1
```
Input: numbers = [2,7,11,15], target = 9
Output: [1,2]
Explanation: The sum of 2 and 7 is 9. Therefore, index1 = 1, index2 = 2. We return [1, 2].
```

Example 2
```
Input: numbers = [2,3,4], target = 6
Output: [1,3]
Explanation: The sum of 2 and 4 is 6. Therefore index1 = 1, index2 = 3. We return [1, 3].
```

Example 3
```
Input: numbers = [-1,0], target = -1
Output: [1,2]
Explanation: The sum of -1 and 0 is -1. Therefore index1 = 1, index2 = 2. We return [1, 2].
```

Constraints
```
2 <= numbers.length <= 3 * 104
-1000 <= numbers[i] <= 1000
numbers is sorted in non-decreasing order.
-1000 <= target <= 1000
The tests are generated such that there is exactly one solution.
```

<!-- more -->

Solution: Maintain 2 pointers, one from the beginning of the array and the other one from the end.
For each loop, compute the sum of `nums[i]` and `nums[j]`. If the sum is smaller than the target,
that means you need a bigger number. Since the array is already sort, move `i` up one position.
Otherwise, decrease `j` by 1. Repeat until you find the correct pair.

Working code in C#

```csharp
public class Solution
{
    public int[] TwoSum(int[] numbers, int target)
    {
        var i = 0;
        var j = numbers.Length - 1;
        while (i < j)
        {
            var sum = numbers[i] + numbers[j];
            if (sum == target)
                return new[] { i+1, j+1 };
            if (sum > target)
                j--;
            else
                i++;
        }

        return Array.Empty<int>();
    }
}
```
