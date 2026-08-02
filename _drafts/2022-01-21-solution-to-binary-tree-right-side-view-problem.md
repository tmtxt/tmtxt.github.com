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

Standing on the right side (the eye), you only see the rightmost node of each level. The dashed
sightlines reach the highlighted nodes `1`, `3`, `4` - the right side view:

<figure style="margin:0">
<svg width="500" height="300" viewBox="0 0 500 300" xmlns="http://www.w3.org/2000/svg" font-family="sans-serif" style="max-width:100%;height:auto">
  <defs>
    <marker id="sight" markerWidth="9" markerHeight="9" refX="7" refY="3" orient="auto">
      <path d="M0,0 L7,3 L0,6 Z" fill="#c62828"/>
    </marker>
  </defs>
  <!-- tree edges -->
  <g stroke="#555" stroke-width="2">
    <line x1="200" y1="50"  x2="120" y2="150"/>
    <line x1="200" y1="50"  x2="280" y2="150"/>
    <line x1="120" y1="150" x2="160" y2="250"/>
    <line x1="280" y1="150" x2="320" y2="250"/>
  </g>
  <!-- sightlines from the eye -->
  <g stroke="#c62828" stroke-width="2" stroke-dasharray="5 4" fill="none" marker-end="url(#sight)">
    <line x1="432" y1="150" x2="224" y2="52"/>
    <line x1="432" y1="150" x2="304" y2="150"/>
    <line x1="432" y1="150" x2="344" y2="248"/>
  </g>
  <!-- eye -->
  <g>
    <ellipse cx="458" cy="150" rx="20" ry="11" fill="#ffffff" stroke="#333" stroke-width="1.5"/>
    <circle cx="452" cy="150" r="6" fill="#333"/>
    <text x="458" y="185" fill="#555" font-size="12" text-anchor="middle">view</text>
  </g>
  <!-- nodes -->
  <g stroke="#333" stroke-width="1.5">
    <circle cx="200" cy="50"  r="22" fill="#ffd54f"/>
    <circle cx="120" cy="150" r="22" fill="#cfe8ff"/>
    <circle cx="280" cy="150" r="22" fill="#ffd54f"/>
    <circle cx="160" cy="250" r="22" fill="#cfe8ff"/>
    <circle cx="320" cy="250" r="22" fill="#ffd54f"/>
  </g>
  <g fill="#333" font-size="18" text-anchor="middle" dominant-baseline="central">
    <text x="200" y="50">1</text>
    <text x="120" y="150">2</text>
    <text x="280" y="150">3</text>
    <text x="160" y="250">5</text>
    <text x="320" y="250">4</text>
  </g>
</svg>
</figure>

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
