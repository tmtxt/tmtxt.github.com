---
layout: post
title: "Solution to Middle of the Linked List problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

Given the `head` of a **singly linked list**, return the middle node of the linked list.

If there are two middle nodes, return the second middle node.

Example 1

![Alt Text](/files/2021-11-27-solution-to-middle-of-the-linked-list-problem/lc-midlist1.jpg)

```
Input: head = [1,2,3,4,5]
Output: 3
Explanation: The middle node of the list is node 3.
```

Example 2

![Alt Text](/files/2021-11-27-solution-to-middle-of-the-linked-list-problem/lc-midlist2.jpg)

```
Input: head = [1,2,3,4,5,6]
Output: 4
Explanation: Since the list has two middle nodes with values 3 and 4, we return the second one.
```

Constraints
```
The number of nodes in the list is in the range [1, 100].
1 <= Node.val <= 100
```

<!-- more -->

Code (C#)

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
public class Solution {
    public ListNode MiddleNode(ListNode head) {
        var slow = head;
        var fast = head;

        while (fast != null && fast.next != null) {
            slow = slow.next;
            fast = fast.next.next;
        }
        return slow;
    }
}
```
