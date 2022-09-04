---
layout: post
title: "Solution to Inorder Successor in Binary Search Tree problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

Write an algorithm to find the **next** node (i.e., in-order successor) of a given node in a binary
search tree. You may assume that each node has a link to its parent.

![BST](/files/2022-02-03-solution-to-inorder-successor-in-binary-search-tree-problem/1200px-Binary_search_tree.png)

- Next node of `3` is `4`
- Next node of `7` is `8`
- Next node of `13` is `14`
- Next node of `14` is `null`

<!-- more -->

**Solution**: Let's break it down into 2 cases

- `1` If the node has a right child, that means the next item must be in the right subtree, or more
  precisely, the next item should be the left-most item in the right subtree (the min item of the
  right subtree).
  - For this first case, we need a helper function to find the min value of a given tree
- `2` If the node doesn't have a right child, that means, the next item should be one of the parent
  nodes (or maybe `null` if this is the maximum value).
  - `2.1` If this node is the left child of its parent, the next value is the parent node
  - `2.2` Otherwise, if this node is the right child of its parent, its value will be greater than its
    parent. In this case, the next item could be in an upper level. We need to traverse up until we
    find a node that is the left child of its parent (the `2.1` case).

Working code in C#

```csharp
public class Solution
{
    public TreeNode InorderSuccessor(TreeNode n)
    {
        // case 1
        if (n.right != null)
            return FindMin(n.right);

        // case 2, traverse up until we find a node that is the left child
        // of its parent
        var p = n.parent;
        while (p != null && p.right == n)
        {
            n = p;
            p = n.parent;
        }

        return p;
    }

    // helper function for case 1, find the left-most item
    public TreeNode FindMin(TreeNode root)
    {
        while (root.left != null)
            root = root.left;
        return root;
    }
}

public class TreeNode
{
    public int val;
    public TreeNode left;
    public TreeNode right;
    public TreeNode parent;
}
```

Test code with the above tree's data

```csharp
class Program
{
    static void Main(string[] args)
    {
        var n8 = new TreeNode { val = 8 };
        var n3 = new TreeNode { val = 3 };
        var n10 = new TreeNode { val = 10 };
        var n1 = new TreeNode { val = 1 };
        var n6 = new TreeNode { val = 6 };
        var n14 = new TreeNode { val = 14 };
        var n4 = new TreeNode { val = 4 };
        var n7 = new TreeNode { val = 7 };
        var n13 = new TreeNode { val = 13 };

        n8.left = n3;
        n8.right = n10;
        n3.left = n1;
        n3.right = n6;
        n10.right = n14;
        n6.right = n7;
        n6.left = n4;
        n14.left = n13;

        n4.parent = n6;
        n7.parent = n6;
        n13.parent = n14;
        n1.parent = n3;
        n6.parent = n3;
        n14.parent = n10;
        n3.parent = n8;
        n10.parent = n8;

        var sol = new Solution();
        // input the node you want to find next successor here
        var next = sol.InorderSuccessor(n14);
        Console.WriteLine(next);
    }
}
```
