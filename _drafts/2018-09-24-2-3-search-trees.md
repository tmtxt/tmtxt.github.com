---
layout: post
title: "2-3 search trees"
description: ""
categories: [algorithm]
thumbnail: /files/2018-09-10-symbol-tables-and-binary-search-trees-summary/bst1.png
mermaid: true
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course.

# Compare to BST

- [Symbol Tables and Binary Search Trees summary]({% post_url 2018-09-23-symbol-tables-and-binary-search-trees-summary %})

<table class="table">
  <thead>
    <tr>
      <th rowspan="2">implementation</th>
      <th colspan="3">worst-case cost<br>(after N inserts)</th>
      <th colspan="3">average case<br>(after N random inserts)</th>
      <th rowspan="2">ordered<br/>iteration?</th>
    </tr>
    <tr>
      <th>search</th>
      <th>insert</th>
      <th>delete</th>
      <th>search hit</th>
      <th>insert</th>
      <th>delete</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>sequential search (unordered list)</td>
      <td>N</td>
      <td>N</td>
      <td>N</td>
      <td>N/2</td>
      <td>N</td>
      <td>N/2</td>
      <td>no</td>
    </tr>
    <tr>
      <td>binary search (ordered array)</td>
      <td>lg N</td>
      <td>N</td>
      <td>N</td>
      <td>lg N</td>
      <td>N/2</td>
      <td>N/2</td>
      <td>yes</td>
    </tr>
    <tr>
      <td>BST</td>
      <td>N</td>
      <td>N</td>
      <td>N</td>
      <td>1.39 lg N</td>
      <td>1.39 lg N</td>
      <td>?</td>
      <td>yes</td>
    </tr>
    <tr>
      <td>goal</td>
      <td>log N</td>
      <td>log N</td>
      <td>log N</td>
      <td>log N</td>
      <td>log N</td>
      <td>log N</td>
      <td>yes</td>
    </tr>
  </tbody>
</table>

# 2-3 tree

A 2-3 tree is a tree that guarantees `log N` search/insert/delete by allowing a node to hold **1
or 2 keys** instead of just 1.

- **2-node**: one key, two children (same as a regular BST node).
- **3-node**: two keys, three children (smaller, in the middle and larger).
- **Symmetric order**: an in-order traversal still yields the keys in ascending order.
- **Perfect balance**: every path from the root to a null link has the *same* length.

<div class="mermaid">
graph TD
  T((T)) --> FN(("F, N"))
  T --> W((W))
  FN --> BD(("B, D"))
  FN --> K((K))
  FN --> Q((Q))
  W --> V((V))
  W --> YZ(("Y, Z"))

  style FN fill:#fde2e2,stroke:#b33,stroke-width:2px
  style BD fill:#fde2e2,stroke:#b33,stroke-width:2px
  style YZ fill:#fde2e2,stroke:#b33,stroke-width:2px
</div>

`F,N`, `B,D` and `Y,Z` are 3-nodes (2 keys, drawn in red above); the rest are 2-nodes. Following
the `F,N` node: the left link leads to keys smaller than `F`, the middle link to keys between `F`
and `N`, and the right link to keys larger than `N` - same idea as a 3-way BST node.

## Search

- Compare the search key against the keys in the node.
- Find the interval containing the search key.
- Follow the associated link, recursively.

Example: searching for `K` walks `T -> F,N -> K`:

<div class="mermaid">
graph TD
  T((T)) --> FN(("F, N"))
  T --> W((W))
  FN --> BD(("B, D"))
  FN --> K((K))
  FN --> Q((Q))
  W --> V((V))
  W --> YZ(("Y, Z"))

  style T fill:#ffd966
  style FN fill:#ffd966
  style K fill:#ffd966
  linkStyle 0 stroke:#e07b00,stroke-width:3px
  linkStyle 3 stroke:#e07b00,stroke-width:3px
</div>

## Insertion

Insertion always happens at the bottom (a leaf). Inserting into a 3-node at the bottom works like
this:

- Add the new key to the 3-node, creating a **temporary 4-node**.
- Move the *middle* key of the 4-node up into the parent.
- Repeat up the tree, as necessary, since pushing a key into the parent can turn the parent into a
  temporary 4-node too.
- If the split reaches the root and the root itself is a 4-node, split it into three 2-nodes - this
  is the only way the tree grows taller.

## Local transformation: splitting a 4-node

Splitting a temporary 4-node is a **local** transformation - a constant number of nodes/links
change, regardless of the size of the tree:

<div class="mermaid">
graph TD
  subgraph Before["Before: temporary 4-node q,r,s"]
    direction TB
    PT(("p, t")) --> QRS(("q, r, s"))
    PT --> L1("&lt; p")
    PT --> L6("&gt; t")
    QRS --> L2("p..q")
    QRS --> L3("q..r")
    QRS --> L4("r..s")
    QRS --> L5("s..t")
  end
</div>

<div class="mermaid">
graph TD
  subgraph After["After: split into three 2-nodes"]
    direction TB
    PRT(("p, r, t")) --> Q((q))
    PRT --> S((s))
    PRT --> M1("&lt; p")
    PRT --> M6("&gt; t")
    Q --> M2("p..q")
    Q --> M3("q..r")
    S --> M4("r..s")
    S --> M5("s..t")
  end
</div>

The middle key `r` moves up one level into the parent (as `p, r, t`), while `q` and `s` become new
2-nodes hanging below it.

## Global properties

Since every transformation is local and preserves symmetric order + perfect balance, the whole tree
stays sorted and balanced no matter where the temporary 4-node appears:

- **Root is a 4-node** - split into three 2-nodes; this is the only case where the tree height grows.

  <div class="mermaid">
  graph TD
    subgraph RootBefore["root: temporary 4-node"]
      PQR(("p, q, r"))
    end
    subgraph RootAfter["root: split"]
      Q2((q)) --> P2((p))
      Q2 --> R2((r))
    end
  </div>

- **Parent is a 2-node** - the 4-node is either the parent's left or right child; the parent simply
  absorbs the middle key and becomes a 3-node. No further splitting is needed.
- **Parent is a 3-node** - the 4-node is the parent's left, middle, or right child; the parent
  absorbs the middle key and itself becomes a temporary 4-node, so the split repeats one level up.

Because each split only ever moves one key up per level, an insertion costs at most `O(tree height)`
splits.

## Performance

- **Worst case height**: `lg N` - a tree made entirely of 2-nodes (behaves like a plain BST).
- **Best case height**: `log₃ N ≈ 0.631 lg N` - a tree made entirely of 3-nodes.
- Between 12 and 20 for a million nodes.
- Between 18 and 30 for a billion nodes.
- Guaranteed **logarithmic** performance for both search and insert, no matter the insertion order.

## Why not implement it directly?

Direct implementation is complicated because:

- Maintaining multiple node types (2-node vs 3-node) is cumbersome.
- Multiple compares are needed just to move down the tree.
- You need to move back up the tree to split 4-nodes.
- There's a large number of cases for splitting (see the six cases above).

In practice, 2-3 trees are usually implemented indirectly through **left-leaning red-black BSTs**,
which encode a 3-node as two 2-nodes joined by a left-leaning red link - same performance
guarantees, much simpler code. That's a topic for another post.

