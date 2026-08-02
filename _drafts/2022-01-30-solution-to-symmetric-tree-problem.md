---
layout: post
title: "Solution to Symmetric Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
mermaid: true
---

> Leetcode: [Symmetric Tree](https://leetcode.com/problems/symmetric-tree/)

Given the `root` of a binary tree, check whether it is a mirror of itself (i.e., symmetric around its center).

**Example 1**:

This tree is a mirror of itself around the center, so the answer is `true`:

<div class="mermaid">
graph TD
    r1["1"] --> l2["2"]
    r1 --> rr2["2"]
    l2 --> l3["3"]
    l2 --> l4["4"]
    rr2 --> r4["4"]
    rr2 --> r3["3"]
</div>

```
Input: root = [1,2,2,3,4,4,3]
Output: true
```

**Example 2**:

Both `2` nodes have their `3` child on the **right** (highlighted red). A mirror would need them on
opposite sides, so the tree is not symmetric and the answer is `false`:

<div class="mermaid">
graph TD
    r1["1"] --> l2["2"]
    r1 --> rr2["2"]
    l2 ~~~ lh1[" "]
    l2 --> l3["3"]:::x
    rr2 ~~~ rh1[" "]
    rr2 --> r3["3"]:::x
    classDef x fill:#ffcdd2,stroke:#c62828,stroke-width:2px
    classDef hidden fill:transparent,stroke:transparent,color:transparent
    class lh1,rh1 hidden
</div>

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
