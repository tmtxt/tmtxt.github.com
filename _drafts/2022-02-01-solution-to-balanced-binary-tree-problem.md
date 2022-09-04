---
layout: post
title: "Solution to Balanced Binary Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Balanced Binary Tree](https://leetcode.com/problems/balanced-binary-tree/)

Given a binary tree, determine if it is height-balanced.

For this problem, a height-balanced binary tree is defined as:

> a binary tree in which the left and right subtrees of every node differ in height by no more than 1.

**Example 1**:

![Balance1](/files/2022-02-01-solution-to-balanced-binary-tree-problem/balance_1.jpg)

```
Input: root = [3,9,20,null,null,15,7]
Output: true
```

**Example 2**:

![Balance2](/files/2022-02-01-solution-to-balanced-binary-tree-problem/balance_2.jpg)

```
Input: root = [1,2,2,3,3,null,null,4,4]
Output: false
```

**Example 3**:

```
Input: root = []
Output: true
```

**Constraints**:
- The number of nodes in the tree is in the range `[0, 5000]`.
- `-104 <= Node.val <= 104`

<!-- more -->

**Easy solution**: Simply use recursion to check if each sub tree is balanced

Working code in C#

```csharp
class Solution
{
    private int GetHeight(TreeNode root)
    {
        if (root == null) return 0;
        return Math.Max(GetHeight(root.left), GetHeight(root.right)) + 1;
    }

    public bool IsBalanced(TreeNode root)
    {
        if (root == null)
            return true;

        if (Math.Abs(GetHeight(root.left) - GetHeight(root.right)) > 1)
            return false;

        return IsBalanced(root.left) && IsBalanced(root.right);
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

Of course, this solution is not efficient. It has to recurse the whole sub tree twice (via
`GetHeight` and `IsBalanced` methods). We can optimize this by also check whether the tree is
balanced while getting its height. If it's not balanced, simply return an invalid number for the
height (`-1` for example).

Working code in C#

```csharp
class Solution
{
    private int GetHeight(TreeNode root)
    {
        // base case, keep the same
        if (root == null) return 0;

        // validate left subtree
        var leftHeight = GetHeight(root.left);
        if (leftHeight == -1)
            return leftHeight;

        // validate right subtree
        var rightHeight = GetHeight(root.right);
        if (rightHeight == -1)
            return rightHeight;

        // validate current root
        if (Math.Abs(leftHeight - rightHeight) > 1)
            return -1;

        return Math.Max(leftHeight, rightHeight) + 1;
    }

    public bool IsBalanced(TreeNode root)
    {
        return GetHeight(root) != -1;
    }
}
```
