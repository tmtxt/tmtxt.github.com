---
layout: post
title: "Solution to Binary Tree Right Side View problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Binary Tree Right Side View](https://leetcode.com/problems/binary-tree-right-side-view/)

Given the `root` of a binary tree, imagine yourself standing on the **right side** of it, return
*the values of the nodes you can see ordered from top to bottom*.

**Example 1**:

![Tree](/files/2022-01-21-solution-to-binary-tree-right-side-view-problem/tree.jpg)

```
Input: root = [1,2,3,null,5,null,4]
Output: [1,3,4]
```

**Example 2**:

```
Input: root = [1,null,3]
Output: [1,3]
```

**Example 3**:

```
Input: root = []
Output: []
```

**Constraints**:
- The number of nodes in the tree is in the range `[0, 100]`.
- `-100 <= Node.val <= 100`

<!-- more -->

**Solution**: Use a modified version of BFS to traverse the tree. When adding item to the queue, add
the right most item first. For each level of the tree, select only the first item to add to the
result list

Working code in C#

```csharp
public class Solution
{
    public IList<int> RightSideView(TreeNode root)
    {
        if (root == null)
            return new List<int>();

        var res = new List<int>();
        // store the node and its depth (level) to the queue
        var q = new Queue<(TreeNode, int)>();
        q.Enqueue((root, 0));
        var lastLevel = -1;

        while (q.Any())
        {
            var (node, level) = q.Dequeue();

            // select only the first item of each tree level to add to the result 
            if (level > lastLevel)
            {
                res.Add(node.val);
                lastLevel++;
            }

            if (node.right != null)
                q.Enqueue((node.right, level + 1));
            if (node.left != null)
                q.Enqueue((node.left, level + 1));
        }

        return res;
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
