---
layout: post
title: "Solution to Convert Sorted Array to Binary Search Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
mermaid: true
---

> Leetcode: [Convert Sorted Array to Binary Search Tree](https://leetcode.com/problems/convert-sorted-array-to-binary-search-tree/)

Given an integer array `nums` where the elements are sorted in **ascending order**, convert it to a
**height-balanced** binary search tree.

A **height-balanced** binary tree is a binary tree in which the depth of the two subtrees of every
node never differs by more than one.

**Example 1**:

Both of these height-balanced BSTs are accepted outputs for `nums = [-10,-3,0,5,9]`.

**Output** `[0,-3,9,-10,null,5]`

<div class="mermaid">
graph TD
    a0["0"] --> am3["-3"]
    a0 --> a9["9"]
    am3 --> am10["-10"]
    am3 ~~~ ah1[" "]
    a9 --> a5["5"]
    a9 ~~~ ah2[" "]
    classDef hidden fill:transparent,stroke:transparent,color:transparent
    class ah1,ah2 hidden
</div>

**Also accepted** `[0,-10,5,null,-3,null,9]`

<div class="mermaid">
graph TD
    b0["0"] --> bm10["-10"]
    b0 --> b5["5"]
    bm10 ~~~ bh1[" "]
    bm10 --> bm3["-3"]
    b5 ~~~ bh2[" "]
    b5 --> b9["9"]
    classDef hidden fill:transparent,stroke:transparent,color:transparent
    class bh1,bh2 hidden
</div>

```
Input: nums = [-10,-3,0,5,9]
Output: [0,-3,9,-10,null,5]
Explanation: [0,-10,5,null,-3,null,9] is also accepted:
```

**Example 2**:

For `nums = [1,3]`, both `[3,1]` and `[1,3]` are valid height-balanced BSTs:

<div class="mermaid">
graph TD
    c3["3"] --> c1["1"]
    c3 ~~~ ch1[" "]
    classDef hidden fill:transparent,stroke:transparent,color:transparent
    class ch1 hidden
</div>

<div class="mermaid">
graph TD
    d1["1"] ~~~ dh1[" "]
    d1 --> d3["3"]
    classDef hidden fill:transparent,stroke:transparent,color:transparent
    class dh1 hidden
</div>

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
