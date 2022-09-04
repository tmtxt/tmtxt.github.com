---
layout: post
title: "Solution to Symmetric Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Symmetric Tree](https://leetcode.com/problems/symmetric-tree/)

Given the `root` of a binary tree, check whether it is a mirror of itself (i.e., symmetric around its center).

**Example 1**:

![sample1](/files/2022-01-30-solution-to-symmetric-tree-problem/symtree1.jpg)

```
Input: root = [1,2,2,3,4,4,3]
Output: true
```

**Example 2**:

![sample1](/files/2022-01-30-solution-to-symmetric-tree-problem/symtree1.jpg)

```
Input: root = [1,2,2,null,3,null,3]
Output: false
```

**Constraints**:
- The number of nodes in the tree is in the range `[1, 1000]`.
- `-100 <= Node.val <= 100`

<!-- more -->

**Solution**: Use BFS on the left and right subtree to traverse and compare each node. For the left
subtree, enqueue the `left` child node first and then the `right` one. For the right subtree, do the
reverse way, enqueue the `right` child node first and then the `left` one.

Working code in C#

```csharp
public class Solution
{
    public bool IsSymmetric(TreeNode root)
    {
        if (root == null)
            return false;

        if (root.left?.val != root.right?.val)
            return false;

        var q1 = new Queue<TreeNode>();
        if (root.left != null)
            q1.Enqueue(root.left);

        var q2 = new Queue<TreeNode>();
        if (root.right != null)
            q2.Enqueue(root.right);

        // either queue has item
        while (q1.Any() || q2.Any())
        {
            // just one queue has item
            if (q1.Any() ^ q2.Any())
                return false;

            // both queue have item
            var node1 = q1.Dequeue();
            var node2 = q2.Dequeue();

            if (node1.left?.val != node2.right?.val || node1.right?.val != node2.left?.val)
                return false;

            if (node1.left != null)
                q1.Enqueue(node1.left);
            if (node1.right != null)
                q1.Enqueue(node1.right);

            if (node2.right != null)
                q2.Enqueue(node2.right);
            if (node2.left != null)
                q2.Enqueue(node2.left);
        }

        return true;
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
