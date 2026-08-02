---
layout: post
title: "Solution to Populating Next Right Pointers in Each Node problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [Populating Next Right Pointers in Each Node](https://leetcode.com/problems/populating-next-right-pointers-in-each-node/)

You are given a **perfect binary tree** where all leaves are on the same level, and every parent has
two children. The binary tree has the following definition:

```c
struct Node {
  int val;
  Node *left;
  Node *right;
  Node *next;
}
```

Populate each next pointer to point to its next right node. If there is no next right node, the next
pointer should be set to `NULL`. Initially, all next pointers are set to `NULL`.

**Example 1**:

Solid lines are the tree's child links; the dashed red arrows are the `next` pointers we populate,
connecting each node to the node on its right in the same level (the last node of each level points
to `NULL`):

<figure style="margin:0">
<svg width="600" height="300" viewBox="0 0 600 300" xmlns="http://www.w3.org/2000/svg" font-family="sans-serif" font-size="18">
  <!-- tree edges -->
  <g stroke="#555" stroke-width="2">
    <line x1="280" y1="50" x2="140" y2="150"/>
    <line x1="280" y1="50" x2="420" y2="150"/>
    <line x1="140" y1="150" x2="70"  y2="250"/>
    <line x1="140" y1="150" x2="210" y2="250"/>
    <line x1="420" y1="150" x2="350" y2="250"/>
    <line x1="420" y1="150" x2="490" y2="250"/>
  </g>
  <!-- next pointers -->
  <defs>
    <marker id="nextArrow" markerWidth="8" markerHeight="8" refX="6" refY="3" orient="auto">
      <path d="M0,0 L6,3 L0,6 Z" fill="#c62828"/>
    </marker>
  </defs>
  <g stroke="#c62828" stroke-width="2" stroke-dasharray="5 4" fill="none" marker-end="url(#nextArrow)">
    <line x1="164" y1="150" x2="392" y2="150"/>
    <line x1="94"  y1="250" x2="182" y2="250"/>
    <line x1="234" y1="250" x2="322" y2="250"/>
    <line x1="374" y1="250" x2="462" y2="250"/>
    <line x1="304" y1="50"  x2="345" y2="50"/>
    <line x1="444" y1="150" x2="485" y2="150"/>
    <line x1="514" y1="250" x2="555" y2="250"/>
  </g>
  <g fill="#c62828" font-size="13">
    <text x="349" y="54">NULL</text>
    <text x="489" y="154">NULL</text>
    <text x="559" y="254">NULL</text>
  </g>
  <!-- nodes -->
  <g fill="#cfe8ff" stroke="#333" stroke-width="1.5">
    <circle cx="280" cy="50"  r="24"/>
    <circle cx="140" cy="150" r="24"/><circle cx="420" cy="150" r="24"/>
    <circle cx="70"  cy="250" r="24"/><circle cx="210" cy="250" r="24"/><circle cx="350" cy="250" r="24"/><circle cx="490" cy="250" r="24"/>
  </g>
  <g fill="#333" text-anchor="middle" dominant-baseline="central">
    <text x="280" y="50">1</text>
    <text x="140" y="150">2</text><text x="420" y="150">3</text>
    <text x="70"  y="250">4</text><text x="210" y="250">5</text><text x="350" y="250">6</text><text x="490" y="250">7</text>
  </g>
</svg>
</figure>

```
Input: root = [1,2,3,4,5,6,7]
Output: [1,#,2,3,#,4,5,6,7,#]
Explanation: Given the above perfect binary tree (Figure A), your function should populate each next pointer to point to its next right node, just like in Figure B. The serialized output is in level order as connected by the next pointers, with '#' signifying the end of each level.
```

**Example 2**:
```
Input: root = []
Output: []
```

**Constraints**:
- The number of nodes in the tree is in the range [0, 212 - 1].
- -1000 <= Node.val <= 1000

**Follow-up**:
- You may only use constant extra space.
- The recursive approach is fine. You may assume implicit stack space does not count as extra space for this problem.

<!-- more -->

**Solution**: Use BFS to traverse the tree and a counter to count the current node in the current
level. The total nodes in one level equal to `Math.Pow(2, level)`.

Sample working code in C#

```csharp
/*
// Definition for a Node.
public class Node {
    public int val;
    public Node left;
    public Node right;
    public Node next;

    public Node() {}

    public Node(int _val) {
        val = _val;
    }

    public Node(int _val, Node _left, Node _right, Node _next) {
        val = _val;
        left = _left;
        right = _right;
        next = _next;
    }
}
*/

public class Solution

{
    public Node Connect(Node root)
    {
        // edge case
        if (root == null) return root;

        // the Queue used for BFS
        var q = new Queue<Node>();
        q.Enqueue(root);

        double currentLevelCount = 0;
        var currentLevel = 0;

        // BFS loop
        while (q.Any())
        {
            var n = q.Dequeue();
            currentLevelCount++;

            var maxCurrentLevelCount = Math.Pow(2, currentLevel);
            if (currentLevelCount == maxCurrentLevelCount)
            {
                // final node in this level, next point to null
                n.next = null;
                currentLevelCount = 0;
                currentLevel++;
            }
            else
            {
                // not the final node in this level, next point to the right node
                n.next = q.Peek();
            }

            // perfect binary tree, only need to check left
            if (n.left != null)
            {
                q.Enqueue(n.left);
                q.Enqueue(n.right);
            }
        }

        return root;
    }
}
```
