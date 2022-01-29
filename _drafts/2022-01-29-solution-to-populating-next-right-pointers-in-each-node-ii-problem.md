---
layout: post
title: "Solution to Populating Next Right Pointers in Each Node II problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Populating Next Right Pointers in Each Node II](https://leetcode.com/problems/populating-next-right-pointers-in-each-node-ii/)

Given a binary tree

```
struct Node {
  int val;
  Node *left;
  Node *right;
  Node *next;
}
```

Populate each next pointer to point to its next right node. If there is no next right node, the next pointer should be set to `NULL`.

Initially, all next pointers are set to `NULL`.

**Example 1**:

![Sample](/files/2022-01-29-solution-to-populating-next-right-pointers-in-each-node-ii-problem/117_sample.png)

- Input: `root = [1,2,3,4,5,null,7]`
- Output: `[1,#,2,3,#,4,5,7,#]`
- Explanation: Given the above binary tree (Figure A), your function should populate each next pointer
to point to its next right node, just like in Figure B. The serialized output is in level order as
connected by the next pointers, with '#' signifying the end of each level.

**Example 2**:
- Input: `root = []`
- Output: `[]`

**Constraints**:
- The number of nodes in the tree is in the range `[0, 6000]`.
- `-100 <= Node.val <= 100`

**Follow-up**:
- You may only use constant extra space.
- The recursive approach is fine. You may assume implicit stack space does not count as extra space for this problem.

<!-- more -->

**Solution**: Use BFS to traverse the tree. However, at the end of each tree level, enqueue a `NULL`
item to the queue as the next pointer.

Working code in C#

```csharp
public class Solution
{
    public Node Connect(Node root)
    {
        // edge case
        if (root == null)
            return root;

        // first level, need to enqueue a null item at the end
        var q = new Queue<Node>();
        q.Enqueue(root);
        q.Enqueue(null);

        // bfs loop
        while (q.Any())
        {
            var node = q.Dequeue();

            // normal BFS
            if (node != null)
            {
                var next = q.Peek();
                node.next = next;

                // enqueue the child items
                if (node.left != null)
                    q.Enqueue(node.left);

                if (node.right != null)
                    q.Enqueue(node.right);

                continue;
            }

            // null item -> last item in this level
            // check whether the queue has any other items (not the last level)
            if (q.Any())
            {
                q.Enqueue(null);
            }
        }

        return root;
    }
}

public class Node
{
    public int val;
    public Node left;
    public Node right;
    public Node next;

    public Node()
    {
    }

    public Node(int _val)
    {
        val = _val;
    }

    public Node(int _val, Node _left, Node _right, Node _next)
    {
        val = _val;
        left = _left;
        right = _right;
        next = _next;
    }
}
```
