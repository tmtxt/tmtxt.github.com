---
layout: post
title: "Solution to Binary Tree Level Order Traversal problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Binary Tree Level Order Traversal](https://leetcode.com/problems/binary-tree-level-order-traversal/)

Given the `root` of a binary tree, return the level order traversal of its nodes' values. (i.e.,
from left to right, level by level).

**Example 1**:

![Sample](/files/2022-01-30-solution-to-binary-tree-level-order-traversal-problem/tree1.jpg)

```
Input: root = [3,9,20,null,null,15,7]
Output: [[3],[9,20],[15,7]]
```

**Example 2**:

```
Input: root = [1]
Output: [[1]]
```

**Example 3**:

```
Input: root = []
Output: []
```

**Constraints**:
- The number of nodes in the tree is in the range `[0, 2000]`.
- `-1000 <= Node.val <= 1000`

<!-- more -->

**Solution**: Of course, use BFS for level-order traversal. For each node, you need to enqueue it
with the current level value to know when you reach the end of that level

Working code in C#

```csharp
public class Solution
{
    public IList<IList<int>> LevelOrder(TreeNode root)
    {
        if (root == null)
            return new List<IList<int>>();

        var res = new List<IList<int>>();
        var lastLevel = 0;
        IList<int> currentLevelRes = null;

        var q = new Queue<(TreeNode, int)>();
        q.Enqueue((root, 1));

        while (q.Any())
        {
            var (node, currentLevel) = q.Dequeue();

            // beginning of each level
            if (currentLevel != lastLevel)
            {
                if (currentLevelRes != null)
                    res.Add(currentLevelRes);

                lastLevel++;
                currentLevelRes = new List<int>();
            }

            currentLevelRes.Add(node.val);

            if (node.left != null)
                q.Enqueue((node.left, currentLevel + 1));
            if (node.right != null)
                q.Enqueue((node.right, currentLevel + 1));
        }

        res.Add(currentLevelRes);

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
