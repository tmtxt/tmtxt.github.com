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

Solid lines are the tree's child links; the dashed red arrows are the `next` pointers we populate,
connecting each node to the node on its right in the same level (the last node of each level points
to `NULL`):

<figure style="margin:0">
<svg width="560" height="300" viewBox="0 0 560 300" xmlns="http://www.w3.org/2000/svg" font-family="sans-serif" style="max-width:100%;height:auto">
  <defs>
    <marker id="nextArrow2" markerWidth="9" markerHeight="9" refX="7" refY="3" orient="auto">
      <path d="M0,0 L7,3 L0,6 Z" fill="#c62828"/>
    </marker>
  </defs>
  <!-- tree edges -->
  <g stroke="#555" stroke-width="2">
    <line x1="250" y1="50"  x2="140" y2="150"/>
    <line x1="250" y1="50"  x2="400" y2="150"/>
    <line x1="140" y1="150" x2="80"  y2="250"/>
    <line x1="140" y1="150" x2="200" y2="250"/>
    <line x1="400" y1="150" x2="460" y2="250"/>
  </g>
  <!-- next pointers -->
  <g stroke="#c62828" stroke-width="2" stroke-dasharray="5 4" fill="none" marker-end="url(#nextArrow2)">
    <line x1="162" y1="150" x2="376" y2="150"/>
    <line x1="102" y1="250" x2="176" y2="250"/>
    <line x1="222" y1="250" x2="436" y2="250"/>
    <line x1="272" y1="50"  x2="313" y2="50"/>
    <line x1="422" y1="150" x2="463" y2="150"/>
    <line x1="482" y1="250" x2="523" y2="250"/>
  </g>
  <g fill="#c62828" font-size="13">
    <text x="317" y="54">NULL</text>
    <text x="467" y="154">NULL</text>
    <text x="527" y="254">NULL</text>
  </g>
  <!-- nodes -->
  <g fill="#cfe8ff" stroke="#333" stroke-width="1.5">
    <circle cx="250" cy="50"  r="22"/>
    <circle cx="140" cy="150" r="22"/><circle cx="400" cy="150" r="22"/>
    <circle cx="80"  cy="250" r="22"/><circle cx="200" cy="250" r="22"/><circle cx="460" cy="250" r="22"/>
  </g>
  <g fill="#333" text-anchor="middle" dominant-baseline="central" font-size="18">
    <text x="250" y="50">1</text>
    <text x="140" y="150">2</text><text x="400" y="150">3</text>
    <text x="80"  y="250">4</text><text x="200" y="250">5</text><text x="460" y="250">7</text>
  </g>
</svg>
</figure>

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
