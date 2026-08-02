---
layout: post
title: "Solution to Maximum Depth of Binary Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
mermaid: true
---

> Leetcode: [Maximum Depth of Binary Tree](https://leetcode.com/problems/maximum-depth-of-binary-tree/)

Given the `root` of a binary tree, return *its maximum depth*.

A binary tree's **maximum depth** is the number of nodes along the longest path from the root node
down to the farthest leaf node.

**Example 1**:

The tree has 3 levels (each a different color), so the maximum depth is `3`:

<div class="mermaid">
graph TD
    n3["3"]:::l0 --> n9["9"]:::l1
    n3 --> n20["20"]:::l1
    n20 --> n15["15"]:::l2
    n20 --> n7["7"]:::l2
    classDef l0 fill:#ffd54f,stroke:#c62828,stroke-width:2px
    classDef l1 fill:#cfe8ff,stroke:#1565c0,stroke-width:2px
    classDef l2 fill:#c8e6c9,stroke:#2e7d32,stroke-width:2px
</div>

```
Input: root = [3,9,20,null,null,15,7]
Output: 3
```

**Example 2**:
```
Input: root = [1,null,2]
Output: 2
```

**Constraints**:
- The number of nodes in the tree is in the range `[0, 104]`.
- `-100 <= Node.val <= 100`

<!-- more -->

**Solution**: Just a simple DFS traversal, maximum recursion call stack equals to the tree's height

Working code in C#

```csharp
public class Solution
{
    public int MaxDepth(TreeNode root)
    {
        if (root == null)
            return 0;

        return Math.Max(MaxDepth(root.left), MaxDepth(root.right)) + 1;
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
