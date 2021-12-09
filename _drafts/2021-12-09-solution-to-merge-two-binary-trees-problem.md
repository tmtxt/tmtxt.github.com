---
layout: post
title: "Solution to Merge Two Binary Trees problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [Merge Two Binary Trees](https://leetcode.com/problems/merge-two-binary-trees/)

You are given two binary trees `root1` and `root2`.

Imagine that when you put one of them to cover the other, some nodes of the two trees are overlapped
while the others are not. You need to merge the two trees into a new binary tree. The merge rule is
that if two nodes overlap, then sum node values up as the new value of the merged node. Otherwise,
the NOT null node will be used as the node of the new tree.

Return *the merged tree*.

**Note**: The merging process must start from the root nodes of both trees.

**Example 1**:

![Merge](/files/2021-12-09-solution-to-merge-two-binary-trees-problem/merge.jpg)

```
Input: root1 = [1,3,2,5], root2 = [2,1,3,null,4,null,7]
Output: [3,4,5,5,4,null,7]
```

**Example 2**:
```
Input: root1 = [1], root2 = [1,2]
Output: [2,2]
```

**Constraints**:
```
The number of nodes in both trees is in the range [0, 2000].
-104 <= Node.val <= 104
```

<!-- more -->

**Solution**: This solution uses a recursive approach

Working code in C#

```csharp
/**
 * Definition for a binary tree node.
 * public class TreeNode {
 *     public int val;
 *     public TreeNode left;
 *     public TreeNode right;
 *     public TreeNode(int val=0, TreeNode left=null, TreeNode right=null) {
 *         this.val = val;
 *         this.left = left;
 *         this.right = right;
 *     }
 * }
 */
public class Solution {
    public TreeNode MergeTrees(TreeNode root1, TreeNode root2)
    {
        if (root1 == null && root2 == null)
            return null;

        if (root1 != null && root2 == null)
            return root1;

        if (root1 == null && root2 != null)
            return root2;

        var res = new TreeNode(root1.val + root2.val);
        res.left = MergeTrees(root1.left, root2.left);
        res.right = MergeTrees(root1.right, root2.right);
        return res;
    }
}
```
