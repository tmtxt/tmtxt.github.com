---
layout: post
title: "Solution to Reverse Linked List problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
mermaid: true
---

Given the `head` of a singly linked list, reverse the list, and return *the reversed list*.

**Example 1**:

**Input**

<div class="mermaid">
flowchart LR
    a1["1"] --> a2["2"] --> a3["3"] --> a4["4"] --> a5["5"] --> anull["null"]
</div>

**Output**

<div class="mermaid">
flowchart LR
    b5["5"] --> b4["4"] --> b3["3"] --> b2["2"] --> b1["1"] --> bnull["null"]
</div>

```
Input: head = [1,2,3,4,5]
Output: [5,4,3,2,1]
```

**Example 2**:

**Input**

<div class="mermaid">
flowchart LR
    c1["1"] --> c2["2"] --> cnull["null"]
</div>

**Output**

<div class="mermaid">
flowchart LR
    d2["2"] --> d1["1"] --> dnull["null"]
</div>

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
