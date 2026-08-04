---
layout: post
title: "1D Range Search"
description: ""
categories: [algorithm]
mermaid: true
---

> just a blog post for summarising my algorithm learning course.

# 1. What is 1D Range Search?

Think of it as the extension of Symbol Table

- **Range search**: find all keys between `k1` and `k2`.
- **Range count**: the number of keys between `k1` and `k2`.

**Application**: Database queries.

**Geometric**: think of the keys as points on a line - range search/count then just
means finding/counting the points that fall inside a given 1d interval. This shows up directly in
database queries (`WHERE k1 <= key AND key <= k2`).

<svg viewBox="0 0 900 180" width="100%" role="img" aria-label="1D range search with points on a line and query interval D to N" xmlns="http://www.w3.org/2000/svg">
  <line x1="80" y1="90" x2="840" y2="90" stroke="#333" stroke-width="3"/>

  <rect x="270" y="58" width="360" height="64" fill="#d9ead3" stroke="#6aa84f" stroke-width="1.5" rx="6"/>
  <text x="450" y="52" text-anchor="middle" font-size="16" fill="#274e13" font-family="Verdana, sans-serif">query interval [D..N]</text>

  <line x1="290" y1="35" x2="290" y2="145" stroke="#6aa84f" stroke-dasharray="6 4" stroke-width="2"/>
  <line x1="610" y1="35" x2="610" y2="145" stroke="#6aa84f" stroke-dasharray="6 4" stroke-width="2"/>
  <text x="290" y="160" text-anchor="middle" font-size="15" fill="#274e13" font-family="Verdana, sans-serif">D</text>
  <text x="610" y="160" text-anchor="middle" font-size="15" fill="#274e13" font-family="Verdana, sans-serif">N</text>

  <circle cx="130" cy="90" r="7" fill="#1f4e79"/>
  <circle cx="220" cy="90" r="7" fill="#1f4e79"/>
  <circle cx="340" cy="90" r="8" fill="#0b8043"/>
  <circle cx="430" cy="90" r="8" fill="#0b8043"/>
  <circle cx="520" cy="90" r="8" fill="#0b8043"/>
  <circle cx="700" cy="90" r="7" fill="#1f4e79"/>
  <circle cx="790" cy="90" r="7" fill="#1f4e79"/>

  <text x="130" y="74" text-anchor="middle" font-size="15" fill="#222" font-family="Verdana, sans-serif">A</text>
  <text x="220" y="74" text-anchor="middle" font-size="15" fill="#222" font-family="Verdana, sans-serif">C</text>
  <text x="340" y="74" text-anchor="middle" font-size="15" fill="#0b8043" font-family="Verdana, sans-serif">F</text>
  <text x="430" y="74" text-anchor="middle" font-size="15" fill="#0b8043" font-family="Verdana, sans-serif">J</text>
  <text x="520" y="74" text-anchor="middle" font-size="15" fill="#0b8043" font-family="Verdana, sans-serif">M</text>
  <text x="700" y="74" text-anchor="middle" font-size="15" fill="#222" font-family="Verdana, sans-serif">R</text>
  <text x="790" y="74" text-anchor="middle" font-size="15" fill="#222" font-family="Verdana, sans-serif">T</text>

  <text x="450" y="138" text-anchor="middle" font-size="14" fill="#274e13" font-family="Verdana, sans-serif">Points inside [D..N]: F, J, M</text>
</svg>

Here's a small ordered symbol table built by inserting `J C T A M F R` one at a time, followed by a
count and a search for the range `D` to `N`:

| operation         | keys in the table (sorted) |
| ----------------- | -------------------------- |
| insert `J`        | `J`                        |
| insert `C`        | `C J`                      |
| insert `T`        | `C J T`                    |
| insert `A`        | `A C J T`                  |
| insert `M`        | `A C J M T`                |
| insert `F`        | `A C F J M T`              |
| insert `R`        | `A C F J M R T`            |
| count `D` to `N`  | `3`                        |
| search `D` to `N` | `F J M`                    |
{: .table }

# 2. List/Array implementations

Before reaching for a BST, consider the two obvious data structures:

- **Unordered list**: `insert` is O(1) (just append), but a range search/count has to scan every
  key, so it's O(N).
- **Ordered array**: `insert` has to shift elements to keep the array sorted, so it's O(N), but a
  range search/count can binary search for the position of `k1` and `k2` and then just walk (or
  count) the keys in between.

| data structure | insert    | range count | range search  |
| -------------- | --------- | ----------- | ------------- |
| unordered list | 1         | N           | N             |
| ordered array  | N         | log N       | R + log N     |
| **goal**       | **log N** | **log N**   | **R + log N** |
{: .table }

`N` is the number of keys in the table, `R` is the number of keys that match the query. Neither
elementary structure hits the goal row - a balanced BST does, as the next two sections show.

# 3. Range count in a BST

Reuse the `rank(key)` operation from an ordinary [BST]({% post_url 2018-09-23-symbol-tables-and-binary-search-trees-summary %})
(the number of keys strictly less than `key`) to answer a range count in two rank queries:

```java
public int size(Key lo, Key hi)
{
    if (contains(hi)) return rank(hi) - rank(lo) + 1;
    else              return rank(hi) - rank(lo);
}
```

For example, in this BST the rank of each key is shown in parentheses:

<div class="mermaid">
graph TD
  J(("J (3)")) --> C(("C (1)"))
  J --> T(("T (6)"))
  C --> A(("A (0)"))
  C --> F(("F (2)"))
  T --> M(("M (4)"))
  T ~~~ Tpad(( ))
  M ~~~ Mpad(( ))
  M --> R(("R (5)"))

style Tpad fill:transparent,stroke:transparent
style Mpad fill:transparent,stroke:transparent
</div>

Running time: proportional to `log N`.

`rank()` walks a single search path, so `size(lo, hi)` only touches the nodes on the search
path to `lo` plus the nodes on the search path to `hi` - both O(`log N`) in a balanced BST.

# 4. Range search in a BST

Range count only needs two ranks, but range search has to actually collect every matching key. The
recursive strategy prunes whole subtrees that can't contain a match:

- Recursively search the **left** subtree, but only if it could contain a key `>= lo`.
- Check whether the key at the current node falls in `[lo, hi]`; if so, add it to the result.
- Recursively search the **right** subtree, but only if it could contain a key `<= hi`.

```java
private void range(Node x, Key lo, Key hi, Queue<Key> result)
{
    if (x == null) return;

    int cmplo = lo.compareTo(x.key);
    int cmphi = hi.compareTo(x.key);

    if (cmplo < 0)              range(x.left, lo, hi, result);
    if (cmplo <= 0 && cmphi >= 0) result.enqueue(x.key);
    if (cmphi > 0)               range(x.right, lo, hi, result);
}
```

Searching the same tree for the range `[D..N]`:

<div class="mermaid">
graph TD
  J(("J")) --> C(("C"))
  J --> T(("T"))
  C --> A(("A"))
  C --> F(("F"))
  T --> M(("M"))
  T ~~~ Tpad(( ))
  M ~~~ Mpad(( ))
  M --> R(("R"))

classDef inRange fill:#b6d7a8,stroke:#38761d,stroke-width:2px;
classDef compareOnly fill:#f4cccc,stroke:#cc0000,stroke-width:2px;
classDef pruned fill:#eeeeee,stroke:#cccccc,color:#999999;

class J,F,M inRange
class C,T,R compareOnly
class A pruned

style Tpad fill:transparent,stroke:transparent
style Mpad fill:transparent,stroke:transparent

</div>

- Green nodes (`J F M`) fall inside `[D..N]` and are added to the result.
- Red nodes (`C T R`) are compared against but don't match: `C < D` so its left subtree (`A`,
  greyed out) is skipped entirely, and `T > N` so its right subtree is skipped too.
- Grey nodes are never even visited - that's the pruning at work.

Running time: proportional to `R + log N`.

The nodes examined are the search path to `lo`, plus the search path to `hi`, plus the `R`
matches themselves - each of those three pieces is bounded, so the total stays close to
`R + log N` even though the recursion visits the whole tree in the worst case (e.g. a query that
matches every key).

# 5. Summary

Backed by a balanced BST (a red-black BST or a B-tree), 1d range search hits the goal row from
the table above: O(`log N`) insert, O(`log N`) range count, and O(`R + log N`) range search - all
without giving up any of the ordered symbol table's other operations.