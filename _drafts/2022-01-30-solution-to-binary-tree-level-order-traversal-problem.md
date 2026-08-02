---
layout: post
title: "Solution to Binary Tree Level Order Traversal problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
mermaid: true
---

> Leetcode: [Binary Tree Level Order Traversal](https://leetcode.com/problems/binary-tree-level-order-traversal/)

Given the `root` of a binary tree, return the level order traversal of its nodes' values. (i.e.,
from left to right, level by level).

**Example 1**:

Each color is one level. Reading the tree level by level (left to right) gives
`[[3], [9, 20], [15, 7]]`:

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
