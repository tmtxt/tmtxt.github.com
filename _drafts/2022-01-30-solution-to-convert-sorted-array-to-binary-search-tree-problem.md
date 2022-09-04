---
layout: post
title: "Solution to Convert Sorted Array to Binary Search Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Convert Sorted Array to Binary Search Tree](https://leetcode.com/problems/convert-sorted-array-to-binary-search-tree/)

Given an integer array `nums` where the elements are sorted in **ascending order**, convert it to a
**height-balanced** binary search tree.

A **height-balanced** binary tree is a binary tree in which the depth of the two subtrees of every
node never differs by more than one.

**Example 1**:

![Output1](/files/2022-01-30-solution-to-convert-sorted-array-to-binary-search-tree-problem/btree1.jpg)
![Output2](/files/2022-01-30-solution-to-convert-sorted-array-to-binary-search-tree-problem/btree2.jpg)

```
Input: nums = [-10,-3,0,5,9]
Output: [0,-3,9,-10,null,5]
Explanation: [0,-10,5,null,-3,null,9] is also accepted:
```

**Example 2**:

![Output2](/files/2022-01-30-solution-to-convert-sorted-array-to-binary-search-tree-problem/btree2.jpg)

```
Input: nums = [1,3]
Output: [3,1]
Explanation: [1,3] and [3,1] are both a height-balanced BSTs.
```

**Constraints**:
- `1 <= nums.length <= 104`
- `-104 <= nums[i] <= 104`
- `nums` is sorted in a **strictly increasing order**.

<!-- more -->

Working code in C#

```csharp
public class Solution
{
    public TreeNode SortedArrayToBST(int[] nums)
    {
        return SortedArrayToBST(nums, 0, nums.Length - 1);
    }

    private TreeNode SortedArrayToBST(int[] nums, int low, int high)
    {
        if (high < low)
            return null;

        var mid = (low + high) / 2;
        var node = new TreeNode
        {
            val = nums[mid],
            left = SortedArrayToBST(nums, low, mid - 1),
            right = SortedArrayToBST(nums, mid + 1, high)
        };
        return node;
    }
}

public class TreeNode
{
    public int val;
    public TreeNode left;
    public TreeNode right;

    public TreeNode(int val = 0, TreeNode left = null, TreeNode right = null)
    {
        this.val = val;
        this.left = left;
        this.right = right;
    }
}
```
