---
layout: post
title: "Solution to Remove nth Node from end of Linked List problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

Given the `head` of a linked list, remove the `nth` node from the end of the list and return its head.

![Alt Text](/files/2021-11-27-solution-to-remove-nth-node-from-end-of-linked-list-problem/remove_ex1.jpg)

Example 1
```
Input: head = [1,2,3,4,5], n = 2
Output: [1,2,3,5]
```

Example 2
```
Input: head = [1], n = 1
Output: []
```

Example 3
```
Input: head = [1,2], n = 1
Output: [1]
```

Constraints
```
The number of nodes in the list is sz.
1 <= sz <= 30
0 <= Node.val <= 100
1 <= n <= sz
```

<!-- more -->

Solution (C#)

Maintain 2 pointers. The gap between the 2 pointers equal to n. When the first pointer reaches the
end of the list, the second one is the `nth` element from the end.

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
    public ListNode RemoveNthFromEnd(ListNode head, int n)
    {
        var beforeHead = new ListNode
        {
            next = head
        };
        var before = beforeHead;
        var after = beforeHead;
        var count = 0;

        while (after.next != null)
        {
            after = after.next;
            if (count >= n)
            {
                before = before.next;
            }

            count++;
        }

        before.next = before.next?.next;

        return beforeHead.next;
    }
}
```
