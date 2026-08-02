---
layout: post
title: "Solution to Middle of the Linked List problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
mermaid: true
---

> Leetcode: [Middle of the Linked List](https://leetcode.com/problems/middle-of-the-linked-list/)

Given the `head` of a **singly linked list**, return the middle node of the linked list.

If there are two middle nodes, return the second middle node.

Example 1

<div class="mermaid">
flowchart LR
    n1["1"] --> n2["2"] --> n3["3"]:::mid --> n4["4"] --> n5["5"] --> null["null"]
    classDef mid fill:#ffd54f,stroke:#c62828,stroke-width:2px
</div>

```
Input: head = [1,2,3,4,5]
Output: 3
Explanation: The middle node of the list is node 3.
```

Example 2

<div class="mermaid">
flowchart LR
    n1["1"] --> n2["2"] --> n3["3"] --> n4["4"]:::mid --> n5["5"] --> n6["6"] --> null["null"]
    classDef mid fill:#ffd54f,stroke:#c62828,stroke-width:2px
</div>

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

Solution (C#)

Maintain 2 pointers, the `fast` and `slow` pointers. The fast one traverse twice as fast as the slow
one.

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
