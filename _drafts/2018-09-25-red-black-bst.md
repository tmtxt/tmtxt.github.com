---
layout: post
title: "Left-leaning Red-black BST"
description: ""
categories: [algorithm]
mermaid: true
---

> just a blog post for summarising my algorithm learning course.

[2-3 search trees]({% post_url 2018-09-24-2-3-search-trees %})

# 1. From 2-3 trees to red-black BSTs

- [2-3 search trees]({% post_url 2018-09-24-2-3-search-trees %}) give guaranteed `log N`
  search/insert, but implementing 3-nodes directly is annoying: multiple node types, multiple
  compares to move down, and a bunch of cases for splitting.
- **Left-leaning red-black BST (LLRB)**: represent a 2-3 tree as an ordinary BST, and use
  "internal" left-leaning links as glue to hold the two keys of a 3-node together.
- A 3-node `p,q` (`p < q`) has three children - for keys smaller than `p`, between `p` and `q`, and
  larger than `q`:

  <div class="mermaid">
  graph TD
    PQ(("p, q")) --> L1("&lt; p")
    PQ --> L2("p..q")
    PQ --> L3("&gt; q")
  </div>

- It's encoded as two 2-nodes: `q` becomes a plain black node, and `p` hangs off its left as a
  **red** child. The three original children still hang in the same relative positions - `q` keeps
  the `> q` child, and `p` takes the other two. The red link is just bookkeeping - it says "these
  two nodes are really one 3-node":

  <div class="mermaid">
  graph TD
    Q((q)) --> P((p))
    Q --> R3("&gt; q")
    P --> R1("&lt; p")
    P --> R2("p..q")

  linkStyle 0 stroke:#c00,stroke-width:3px

  </div>

- Here's that same idea inside an actual tree. The 2-3 tree below has `H,L` as one of its 3-nodes:

  <div class="mermaid">
  graph TD
    R((R)) --> HL(("H, L"))
    R --> X((X))
    HL --> A((A))
    HL --> J((J))
    HL --> M((M))
  </div>

- ...and its red-black equivalent, where `L` stays a plain black node and `H` hangs off it as a red
  left child:

    <div class="mermaid">
    graph TD
      R((R)) --> L((L))
      R --> X((X))
      L --> H((H))
      L --> M((M))
      H --> A((A))
      H --> J((J))

  linkStyle 2 stroke:#c00,stroke-width:3px

    </div>

A red-black BST is a BST whose links are colored red or black, such that:

- No node has two red links connected to it.
- Every path from the root to a null link has the same number of **black** links ("perfect black
  balance").
- Red links lean left.

Every 2-3 tree corresponds to exactly one LLRB tree: 2-nodes stay as they are, and each 3-node
becomes a black node with a red left child. Take the 2-3 tree from the
[previous post]({% post_url 2018-09-24-2-3-search-trees %}):

<div class="mermaid">
graph TD
  T((T)) --> FN(("F, N"))
  T --> W((W))
  FN --> BD(("B, D"))
  FN --> K((K))
  FN --> Q((Q))
  W --> V((V))
  W --> YZ(("Y, Z"))
</div>

Its corresponding red-black BST looks like this (red links drawn in red):

<div class="mermaid">
graph TD
  T((T)) --> N((N))
  T --> W((W))
  N --> F((F))
  N --> Q((Q))
  F --> D((D))
  F --> K((K))
  D --> B((B))
  D ~~~ Dpad(( ))
  W --> V((V))
  W --> Z((Z))
  Z --> Y((Y))
  Z ~~~ Zpad(( ))

linkStyle 2 stroke:#c00,stroke-width:3px
linkStyle 6 stroke:#c00,stroke-width:3px
linkStyle 10 stroke:#c00,stroke-width:3px
style Dpad fill:transparent,stroke:transparent
style Zpad fill:transparent,stroke:transparent

</div>

Each 3-node from the 2-3 tree (`F,N`, `B,D` and `Y,Z`) turned into a black node with a red left
child (`N`&larr;`F`, `D`&larr;`B`, `Z`&larr;`Y`); every 2-node stayed a plain black node.

# 2. Search

Search is exactly the same as in an elementary BST - the colors are simply ignored, it just
happens to run faster because the tree is better balanced.

Example: searching for `K` walks `T -> N -> F -> K`, crossing the red link between `N` and `F`
along the way:

<div class="mermaid">
graph TD
  T((T)) --> N((N))
  T --> W((W))
  N --> F((F))
  N --> Q((Q))
  F --> D((D))
  F --> K((K))
  D --> B((B))
  D ~~~ Dpad(( ))
  W --> V((V))
  W --> Z((Z))
  Z --> Y((Y))
  Z ~~~ Zpad(( ))

style T fill:#ffd966
style N fill:#ffd966
style F fill:#ffd966
style K fill:#ffd966
style Dpad fill:transparent,stroke:transparent
style Zpad fill:transparent,stroke:transparent
linkStyle 0 stroke:#e07b00,stroke-width:3px
linkStyle 2 stroke:#e07b00,stroke-width:3px
linkStyle 5 stroke:#e07b00,stroke-width:3px

</div>

Most other read-only operations (floor, ceiling, selection, iteration, ...) are also identical to
a plain BST.

```csharp
public string Get(int key)
{
    Node node = root;
    while (node != null)
    {
        int cmp = key.CompareTo(node.Key);
        if (cmp < 0) node = node.Left;
        else if (cmp > 0) node = node.Right;
        else return node.Value;
    }
    return null;
}
```

# 3. Node representation

Since every node is pointed to by exactly one link (from its parent), the color can be stored
_on the node itself_, as the color of the link coming down from its parent:

- Each node stores its key/value, its two children, and a boolean `color` field.
- `color` records whether the link **from the parent** to this node is red or black.
- Null links are considered black.

```csharp
private const bool Red = true;
private const bool Black = false;

private class Node
{
    public int Key;
    public string Value;
    public Node Left;
    public Node Right;
    public bool Color; // color of the link from the parent to this node

    public Node(int key, string value, bool color)
    {
        Key = key;
        Value = value;
        Color = color;
    }
}

private static bool IsRed(Node node)
{
    if (node == null) return false; // null links are black
    return node.Color == Red;
}
```

# 4. Elementary operations

Every red-black BST operation is built from three tiny local operations. Each one preserves
symmetric order and perfect black balance.

## 4.1 Rotations

**Left rotation** - orient a (temporarily) right-leaning red link to lean left:

<div class="mermaid">
graph TD
  G((G)) ~~~ Gpad(( ))
  G --> O((O))

linkStyle 1 stroke:#c00,stroke-width:3px
style Gpad fill:transparent,stroke:transparent

</div>

becomes

<div class="mermaid">
graph TD
  O((O)) --> G((G))
  O ~~~ Opad(( ))

linkStyle 0 stroke:#c00,stroke-width:3px
style Opad fill:transparent,stroke:transparent

</div>

**Right rotation** is just the mirror image - it orients a left-leaning red link to (temporarily)
lean right, turning the "after" picture above back into the "before" one. Both rotations keep the
subtree's in-order sequence unchanged; only the shape and the position of the red link change.

```csharp
private Node RotateLeft(Node h)
{
    Node x = h.Right;
    h.Right = x.Left;
    x.Left = h;
    x.Color = h.Color;
    h.Color = Red;
    return x;
}

private Node RotateRight(Node h)
{
    Node x = h.Left;
    h.Left = x.Right;
    x.Right = h;
    x.Color = h.Color;
    h.Color = Red;
    return x;
}
```

## 4.2 Color flip

Recolors a node and its two children to split a temporary 4-node. Before the flip, a black node
has two red children:

<div class="mermaid">
graph TD
  O((O)) --> G((G))
  O --> U((U))

linkStyle 0 stroke:#c00,stroke-width:3px
linkStyle 1 stroke:#c00,stroke-width:3px

</div>

After the flip, `G` and `U` turn black, and `O` itself turns red (so its _own_ link to its parent
becomes red, ready to be dealt with one level up):

<div class="mermaid">
graph TD
  O((O)) --> G((G))
  O --> U((U))
</div>

```csharp
private void FlipColors(Node h)
{
    h.Color = !h.Color;
    h.Left.Color = !h.Left.Color;
    h.Right.Color = !h.Right.Color;
}
```

# 5. Insertion

The strategy is always the same: do a normal BST insert, attach the new node with a **red** link,
then walk back up the search path fixing any violations using the three operations above.

## 5.1 Case 1: insert into a 2-node

If the new key is smaller, it simply attaches as a red left child - already a valid 3-node, no
fix-up needed. If it's larger, it attaches as a red _right_ child, which is not allowed to lean
right, so a left rotation fixes it.

Going back to the tree above, let's insert `X`, which belongs under the leaf `V`:

<div class="mermaid">
graph TD
  T((T)) --> N((N))
  T --> W((W))
  N --> F((F))
  N --> Q((Q))
  F --> D((D))
  F --> K((K))
  D --> B((B))
  D ~~~ Dpad(( ))
  W --> V((V))
  W --> Z((Z))
  Z --> Y((Y))
  Z ~~~ Zpad(( ))
  V ~~~ Vpad(( ))
  V --> X((X))

linkStyle 2 stroke:#c00,stroke-width:3px
linkStyle 6 stroke:#c00,stroke-width:3px
linkStyle 10 stroke:#c00,stroke-width:3px
linkStyle 13 stroke:#c00,stroke-width:3px
style X fill:#ffcccc,stroke:#c00,stroke-width:3px
style Dpad fill:transparent,stroke:transparent
style Zpad fill:transparent,stroke:transparent
style Vpad fill:transparent,stroke:transparent

</div>

`X` is attached as a red _right_ child of `V` - a temporary, illegal right-leaning red link. A
single left rotation at `V` fixes it: `X` takes `V`'s place under `W`, with `V` hanging off as its
red left child:

<div class="mermaid">
graph TD
  T((T)) --> N((N))
  T --> W((W))
  N --> F((F))
  N --> Q((Q))
  F --> D((D))
  F --> K((K))
  D --> B((B))
  D ~~~ Dpad(( ))
  W --> X((X))
  X --> V((V))
  X ~~~ Xpad(( ))
  W --> Z((Z))
  Z --> Y((Y))
  Z ~~~ Zpad(( ))

linkStyle 2 stroke:#c00,stroke-width:3px
linkStyle 6 stroke:#c00,stroke-width:3px
linkStyle 9 stroke:#c00,stroke-width:3px
linkStyle 12 stroke:#c00,stroke-width:3px
style Dpad fill:transparent,stroke:transparent
style Xpad fill:transparent,stroke:transparent
style Zpad fill:transparent,stroke:transparent

</div>

No violation reaches `W`, so the insertion is done in a single rotation.

## 5.2 Case 2: insert into a 3-node

This is where a rotation _and_ a color flip usually happen together. Take a standalone 3-node
`G,O` (`G` is `O`'s red left child):

<div class="mermaid">
graph TD
  O((O)) --> G((G))
  O ~~~ Opad(( ))

linkStyle 0 stroke:#c00,stroke-width:3px
style Opad fill:transparent,stroke:transparent

</div>

Insert a key `C` smaller than `G`. It attaches as a red left child of `G`:

<div class="mermaid">
graph TD
  O((O)) --> G((G))
  O ~~~ Opad(( ))
  G --> C((C))
  G ~~~ Gpad(( ))

linkStyle 0 stroke:#c00,stroke-width:3px
linkStyle 2 stroke:#c00,stroke-width:3px
style Opad fill:transparent,stroke:transparent
style Gpad fill:transparent,stroke:transparent

</div>

Now there are two left-leaning red links in a row (`O -> G -> C`), which breaks the "no two reds
in a row" rule. A right rotation at `O` fixes the lean - `G` takes `O`'s place, with `C` and `O` as
its two red children:

<div class="mermaid">
graph TD
  G((G)) --> C((C))
  G --> O((O))

linkStyle 0 stroke:#c00,stroke-width:3px
linkStyle 1 stroke:#c00,stroke-width:3px

</div>

`G` now has _two_ red children - a temporary 4-node - so a color flip splits it: `C` and `O` turn
black, and `G` turns red to pass the split one level up (exactly like the middle key moving up
into the parent in a 2-3 tree):

<div class="mermaid">
graph TD
  G((G)) --> C((C))
  G --> O((O))
</div>

## 5.3 Putting it together

Walking back up from the newly inserted node, the same three checks are applied at every node on
the search path:

- Right child red, left child black &rarr; rotate left (straighten a right-leaning link).
- Left child red _and_ left-left grandchild red &rarr; rotate right (fix two lefts in a row).
- Both children red &rarr; flip colors (split a temporary 4-node, pass it up).

Repeating this at each level guarantees the red link either gets absorbed or keeps moving up,
exactly as in a 2-3 tree insertion. If it reaches the root and the root ends up red, it's simply
repainted black - the only case where the tree grows one level taller.

All of this fits in a handful of lines on top of a standard recursive BST insert:

```csharp
public void Put(int key, string value)
{
    root = Put(root, key, value);
    root.Color = Black; // root is always black
}

private Node Put(Node h, int key, string value)
{
    if (h == null) return new Node(key, value, Red); // insert at the bottom, link colored red

    int cmp = key.CompareTo(h.Key);
    if (cmp < 0) h.Left = Put(h.Left, key, value);
    else if (cmp > 0) h.Right = Put(h.Right, key, value);
    else h.Value = value;

    if (IsRed(h.Right) && !IsRed(h.Left)) h = RotateLeft(h);       // lean left
    if (IsRed(h.Left) && IsRed(h.Left.Left)) h = RotateRight(h);   // balance a 4-node
    if (IsRed(h.Left) && IsRed(h.Right)) FlipColors(h);            // split a 4-node

    return h;
}
```

# 6. Performance

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
      <td>2-3 tree</td>
      <td>c lg N</td>
      <td>c lg N</td>
      <td>c lg N</td>
      <td>c lg N</td>
      <td>c lg N</td>
      <td>c lg N</td>
      <td>yes</td>
    </tr>
    <tr>
      <td>red-black BST</td>
      <td>2 lg N</td>
      <td>2 lg N</td>
      <td>2 lg N</td>
      <td>~1.00 lg N</td>
      <td>~1.00 lg N</td>
      <td>~1.00 lg N</td>
      <td>yes</td>
    </tr>
  </tbody>
</table>

- Every path from root to null link has the same number of _black_ links, and no two red links
  ever appear in a row, so the height is at most `2 lg N` in the worst case.
- In typical, non-adversarial use the height tends to be close to `lg N`.

# 7. Why red-black BSTs?

Because they get almost all of the 2-3 tree's balance guarantees while being just a thin,
constant-overhead layer on top of an ordinary BST (a single extra color bit per node, three local
fix-up operations), red-black trees ended up as one of the most widely used balanced search trees
in practice:

- Java's `java.util.TreeMap` / `java.util.TreeSet`.
- C++ STL's `map`, `multimap`, `multiset`.
- The Linux kernel's completely fair scheduler (`linux/rbtree.h`).

`B-trees` take a different route to the same goal - instead of 2 or 3 keys per node, they allow up
to `M - 1`, which is a much better fit for data that lives on disk (databases, file systems) where
minimizing the number of page reads matters more than the number of comparisons. That's a topic
for another post.
