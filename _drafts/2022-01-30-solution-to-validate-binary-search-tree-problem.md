---
layout: post
title: "Solution to Validate Binary Search Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Validate Binary Search Tree](https://leetcode.com/problems/validate-binary-search-tree/)

Given the `root` of a binary tree, determine if it is a valid binary search tree (BST).

A **valid BST** is defined as follows:
- The left subtree of a node contains only nodes with keys **less than** the node's key.
- The right subtree of a node contains only nodes with keys **greater than** the node's key.
- Both the left and right subtrees must also be binary search trees.

**Example 1**:

![Sample1](/files/2022-01-30-solution-to-validate-binary-search-tree-problem/tree1.jpg)

```
Input: root = [2,1,3]
Output: true
```

**Example 2**:

![Sample2](/files/2022-01-30-solution-to-validate-binary-search-tree-problem/tree2.jpg)

```
Input: root = [5,1,4,null,null,3,6]
Output: false
Explanation: The root node's value is 5 but its right child's value is 4.
```

**Constraints**:
- The number of nodes in the tree is in the range `[1, 104]`.
- `-231 <= Node.val <= 231 - 1`

<!-- more -->

**Solution**: If a btree is a valid BST, an in-order traversal will print everything in the correct
order. Convert the btree to an array and check if it's a sorted array.

Working code in C#

```csharp
public class Solution
{
    public bool IsValidBST(TreeNode root)
    {
        // build array from the btree
        var list = new List<int>();
        Traverse(root, list);

        // check if array is sorted
        var arr = list.ToArray();
        if (arr.Length == 1)
            return true;

        for (var i = 1; i < arr.Length; i++)
        {
            if (arr[i] <= arr[i - 1])
                return false;
        }

        return true;
    }

    // In-order traversal and add item to the output list
    private void Traverse(TreeNode root, IList<int> res)
    {
        if (root == null)
            return;

        Traverse(root.left, res);
        res.Add(root.val);
        Traverse(root.right, res);
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
