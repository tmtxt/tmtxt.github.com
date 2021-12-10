---
layout: post
title: "Solution to Reverse Linked List problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

Given the `head` of a singly linked list, reverse the list, and return *the reversed list*.

**Example 1**:

![Reverse1](/files/2021-12-10-solution-to-reverse-linked-list-problem/rev1ex1.jpg)

```
Input: head = [1,2,3,4,5]
Output: [5,4,3,2,1]
```

**Example 2**:

![Reverse2](/files/2021-12-10-solution-to-reverse-linked-list-problem/rev1ex2.jpg)

```
Input: head = [1,2]
Output: [2,1]
```

**Example 3**:

```
Input: head = []
Output: []
```

**Constraints**:
- The number of nodes in the list is the range `[0, 5000]`.
- `-5000 <= Node.val <= 5000`

<!-- more -->

Working code in C#

```csharp
/**
 * Definition for singly-linked list.
 * public class ListNode {
 *     public int val;
 *     public ListNode next;
 *     public ListNode(int val=0, ListNode next=null) {
 *         this.val = val;
 *         this.next = next;
 *     }
 * }
 */

public class Solution
{
    public ListNode ReverseList(ListNode head)
    {
        var current = head;
        ListNode previous = null;

        while (head != null)
        {
            var temp = head.next;
            head.next = previous;
            current = head;
            previous = current;
            head = temp;
        }

        return current;
    }
}
```
