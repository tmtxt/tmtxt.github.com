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

![116 sample](/files/2021-12-09-solution-to-populating-next-right-pointers-in-each-node-problem/116_sample.png)

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
