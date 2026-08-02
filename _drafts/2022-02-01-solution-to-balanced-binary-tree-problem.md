---
layout: post
title: "Solution to Balanced Binary Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
mermaid: true
---

> Leetcode: [Balanced Binary Tree](https://leetcode.com/problems/balanced-binary-tree/)

Given a binary tree, determine if it is height-balanced.

For this problem, a height-balanced binary tree is defined as:

> a binary tree in which the left and right subtrees of every node differ in height by no more than 1.

**Example 1**:

Every node's left and right subtree heights differ by at most 1, so the tree is balanced (`true`):

<div class="mermaid">
graph TD
    n3["3"] --> n9["9"]
    n3 --> n20["20"]
    n20 --> n15["15"]
    n20 --> n7["7"]
</div>

```
Input: root = [3,9,20,null,null,15,7]
Output: true
```

**Example 2**:

At the root `1` (highlighted red) the left subtree has height 3 while the right subtree has height
1 - a difference of 2 - so the tree is not balanced (`false`):

<div class="mermaid">
graph TD
    n1["1"]:::x --> l2["2"]
    n1 --> r2["2"]
    l2 --> l3a["3"]
    l2 --> l3b["3"]
    l3a --> l4a["4"]
    l3a --> l4b["4"]
    classDef x fill:#ffcdd2,stroke:#c62828,stroke-width:2px
</div>

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
