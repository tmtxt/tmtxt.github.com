---
layout: post
title: "Solution to Subtree of Another Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
mermaid: true
---

> Leetcode: [Subtree of Another Tree](https://leetcode.com/problems/subtree-of-another-tree/)

Given the roots of two binary trees `root` and `subRoot`, return `true` if there is a subtree of
`root` with the same structure and node values of `subRoot` and `false` otherwise.

A subtree of a binary tree `tree` is a tree that consists of a node in `tree` and all of this node's
descendants. The tree `tree` could also be considered as a subtree of itself.

**Example 1**:

The subtree rooted at `4` (highlighted green) has exactly the same structure and values as
`subRoot`, so the answer is `true`.

**`root`**

<div class="mermaid">
graph TD
    a3["3"] --> a4["4"]:::m
    a3 --> a5["5"]
    a4 --> a1["1"]:::m
    a4 --> a2["2"]:::m
    classDef m fill:#c8e6c9,stroke:#2e7d32,stroke-width:2px
</div>

**`subRoot`**

<div class="mermaid">
graph TD
    s4["4"] --> s1["1"]
    s4 --> s2["2"]
</div>

```
Input: root = [3,4,5,1,2], subRoot = [4,1,2]
Output: true
```

**Example 2**:

The candidate subtree rooted at `4` almost matches, but node `2` has an extra child `0`
(highlighted red), so it does not match `subRoot` and the answer is `false`.

**`root`**

<div class="mermaid">
graph TD
    b3["3"] --> b4["4"]
    b3 --> b5["5"]
    b4 --> b1["1"]
    b4 --> b2["2"]
    b2 --> b0["0"]:::x
    b2 ~~~ bh1[" "]
    classDef x fill:#ffcdd2,stroke:#c62828,stroke-width:2px
    classDef hidden fill:transparent,stroke:transparent,color:transparent
    class bh1 hidden
</div>

**`subRoot`**

<div class="mermaid">
graph TD
    t4["4"] --> t1["1"]
    t4 --> t2["2"]
</div>

```
Input: root = [3,4,5,1,2,null,null,null,null,0], subRoot = [4,1,2]
Output: false
```

**Constraints**:
- The number of nodes in the `root` tree is in the range `[1, 2000]`.
- The number of nodes in the `subRoot` tree is in the range `[1, 1000]`.
- `-104 <= root.val <= 104`
- `-104 <= subRoot.val <= 104`

<!-- more -->

**Solution**: Find the nodes with the same value with the `subRoot` and then check that subtree
using BFS or DFS. (DFS seems simpler)

Working code in C#

```csharp
public class Solution
{
    public bool IsSubtree(TreeNode root, TreeNode subRoot)
    {
        // find all the sub nodes with the same value with subRoot
        var subNodes = FindSubRoots(root, subRoot);

        // check if any of them has the same structure with subRoot
        return subNodes.Any(subNode => CheckTwoTrees(subNode, subRoot));
    }

    // Check the 2 input trees if they have the same structure
    private bool CheckTwoTrees(TreeNode root1, TreeNode root2)
    {
        // use BFS with 2 queues
        // you can also use just 1 queue to enqueue a tuple Queue<(TreeNode, TreeNode)>
        var q1 = new Queue<TreeNode>();
        var q2 = new Queue<TreeNode>();
        q1.Enqueue(root1);
        q2.Enqueue(root2);

        while (q1.Any() || q2.Any())
        {
            if (q1.Count != q2.Count)
                return false;

            var node1 = q1.Dequeue();
            var node2 = q2.Dequeue();

            if (node1 == null && node2 == null)
                continue;

            if (node1 == null || node2 == null)
                return false;

            // both nodes are not null
            if (node1.val != node2.val)
                return false;

            // enqueue child items
            q1.Enqueue(node1.left);
            q1.Enqueue(node1.right);
            q2.Enqueue(node2.left);
            q2.Enqueue(node2.right);
        }

        return true;
    }

    // Find the node inside "root" tree that has the same value with subRoot
    private IList<TreeNode> FindSubRoots(TreeNode root, TreeNode subRoot)
    {
        var res = new List<TreeNode>();

        if (root == null || subRoot == null)
            return res;

        var q = new Queue<TreeNode>();
        q.Enqueue(root);

        while (q.Any())
        {
            var node = q.Dequeue();
            if (node.val == subRoot.val)
                res.Add(node);

            if (node.left != null)
                q.Enqueue(node.left);

            if (node.right != null)
                q.Enqueue(node.right);
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
