---
layout: post
title: "Solution to Merge Two Sorted Lists problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
mermaid: true
---

> Leetcode: [Merge Two Sorted Lists](https://leetcode.com/problems/merge-two-sorted-lists/)

You are given the heads of two sorted linked lists `list1` and `list2`.

Merge the two lists in a one **sorted** list. The list should be made by splicing together the nodes
of the first two lists.

Return *the head of the merged linked list*.

**Example 1**:

**list1**

<div class="mermaid">
flowchart LR
    a1["1"] --> a2["2"] --> a4["4"] --> anull["null"]
</div>

**list2**

<div class="mermaid">
flowchart LR
    b1["1"] --> b3["3"] --> b4["4"] --> bnull["null"]
</div>

**Merged**

<div class="mermaid">
flowchart LR
    m1["1"] --> m1b["1"] --> m2["2"] --> m3["3"] --> m4["4"] --> m4b["4"] --> mnull["null"]
</div>

```
Input: list1 = [1,2,4], list2 = [1,3,4]
Output: [1,1,2,3,4,4]
```

**Example 2**:

```
Input: list1 = [], list2 = []
Output: []
```

**Example 3**:

```
Input: list1 = [], list2 = [0]
Output: [0]
```

**Constraints**:
- The number of nodes in both lists is in the range `[0, 50]`.
- `-100 <= Node.val <= 100`
- Both `list1` and `list2` are sorted in **non-decreasing** order.

<!-- more -->

**Solution**: Simply use the **Merge** part of Merge Sort

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
    public ListNode MergeTwoLists(ListNode list1, ListNode list2)
    {
        if (list1 == null && list2 == null)
            return null;

        var head = new ListNode();
        var current = head;

        while (list1 != null || list2 != null)
        {
            if (list1 == null)
            {
                current.next = list2;
                list2 = list2.next;
            }
            else if (list2 == null)
            {
                current.next = list1;
                list1 = list1.next;
            }
            else if (list1.val < list2.val)
            {
                current.next = list1;
                list1 = list1.next;
            }
            else
            {
                current.next = list2;
                list2 = list2.next;
            }

            current = current.next;
        }

        return head.next;
    }
}
```
