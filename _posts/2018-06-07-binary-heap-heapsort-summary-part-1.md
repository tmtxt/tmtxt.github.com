---
layout: post
title: "Binary Heap and Heapsort Summary - Part 1 - Binary Heap"
description: ""
categories: [algorithm]
mermaid: true
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Probably
> this was taught in the University but I don't remember anything, I have no idea about its
> definition and applications until I take this course.

# Heap-ordered Binary Tree

<div class="mermaid">
graph TD
    T["T (1)"] --> S["S (2)"]
    T --> R["R (3)"]
    S --> P["P (4)"]
    S --> N["N (5)"]
    R --> O["O (6)"]
    R --> A["A (7)"]
    P --> E["E (8)"]
    P --> I["I (9)"]
    N --> H["H (10)"]
    N --> G["G (11)"]
</div>

- Each node represents a key
- Parent's key is not smaller than children's keys

# Array Representation

Taking the tree above in level order gives the array below. Index `0` is left unused so that the
parent/child arithmetic stays simple:

| Index `k`  | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 |
|------------|---|---|---|---|---|---|---|---|---|----|----|
| Key `a[k]` | T | S | R | P | N | O | A | E | I | H  | G  |
{: .table }

<!-- more -->

- Indices start at 1.
- Take nodes in level order.
- No explicit links needed!
- Largest key is a[1], which is root of binary tree
- Can use array indices to move through tree
  - Parent of node at k is at k/2
  - Children of node at k are at 2k and 2k+1

# Promotion in a heap

- **Scenario**: Child's key becomes larger key than its parent's key.
- To eliminate the violation:
  - Exchange key in child with key in parent.
  - Repeat until heap order restored.
- In the below image, the 5th item `T` is not in the correct order
  - `T` is larger than `P` (its parent), exchange
  - `T` is still larger than `S` (its parent), exchange
  - Finally, `T` is in the correct order

`T` starts at index `5`. Since it is larger than its parent `P` (index `2`) it swims up, and it is
still larger than the new parent `S` (index `1`), so it swims up once more to become the root:

**Before swim:**

<div class="mermaid">
graph TD
    bS["S (1)"] --> bP["P (2)"]
    bS --> bR["R (3)"]
    bP --> bO["O (4)"]
    bP --> bT["T (5)"]:::v
    bR --> bN["N (6)"]
    bR --> bA["A (7)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 0,3 stroke:#c62828,stroke-width:3px
</div>

**After swim:**

<div class="mermaid">
graph TD
    aT["T (1)"]:::v --> aS["S (2)"]
    aT --> aR["R (3)"]
    aS --> aO["O (4)"]
    aS --> aP["P (5)"]
    aR --> aN["N (6)"]
    aR --> aA["A (7)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 0,3 stroke:#c62828,stroke-width:3px
</div>

```java
private void swim(int k) {
    while (k > 1 && less(k/2, k)) {
        exch(k, k/2);
        k = k/2;
    }
}
```

# Insertion in a heap

- Add node at end, then swim it up.
- **Cost**: At most `1 + lgN` compares.

Inserting `T` adds it at the next free slot (index `7`), then swims it up until heap order is
restored:

**New key added at the end:**

<div class="mermaid">
graph TD
    xS["S (1)"] --> xR["R (2)"]
    xS --> xO["O (3)"]
    xR --> xN["N (4)"]
    xR --> xE["E (5)"]
    xO --> xA["A (6)"]
    xO --> xT["T (7)"]:::v
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 1,5 stroke:#c62828,stroke-width:3px
</div>

**After swimming up:**

<div class="mermaid">
graph TD
    yT["T (1)"]:::v --> yR["R (2)"]
    yT --> yS["S (3)"]
    yR --> yN["N (4)"]
    yR --> yE["E (5)"]
    yS --> yA["A (6)"]
    yS --> yO["O (7)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 1,5 stroke:#c62828,stroke-width:3px
</div>

```java
public void insert(Key x) {
    pq[++N] = x;
    swim(N);
}
```

# Demotion in a heap

- **Scenario**: Parent's key becomes smaller than one (or both) of its children's.
- To eliminate the violation:
  - Exchange key in parent with key in larger child.
  - Repeat until heap order restored.
- In the below image, the 2nd item `H` is not in the right order
  - `H` is smaller than its children, exchange with the larger child `S`
  - `H` is still smaller than its children, exchange with the larger child `N`
  - Finally, `H` is in the correct order

`H` starts at index `2`. It is smaller than its children, so it sinks by swapping with the larger
child `S` (index `4`), and then again with the larger child `N` (index `8`):

**Before sink:**

<div class="mermaid">
graph TD
    bT["T (1)"] --> bH["H (2)"]:::v
    bT --> bR["R (3)"]
    bH --> bS["S (4)"]
    bH --> bG["G (5)"]
    bR --> bO["O (6)"]
    bR --> bA["A (7)"]
    bS --> bN["N (8)"]
    bS --> bE["E (9)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 2,6 stroke:#c62828,stroke-width:3px
</div>

**After sink:**

<div class="mermaid">
graph TD
    aT["T (1)"] --> aS["S (2)"]
    aT --> aR["R (3)"]
    aS --> aN["N (4)"]
    aS --> aG["G (5)"]
    aR --> aO["O (6)"]
    aR --> aA["A (7)"]
    aN --> aH["H (8)"]:::v
    aN --> aE["E (9)"]
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    linkStyle 2,6 stroke:#c62828,stroke-width:3px
</div>

```java
private void sink(int k) {
    while (2*k <= N) {
        int j = 2*k;
        // children of node at k are 2k and 2k+1, decide which one is larger
        if (j < N && less(j, j+1)) j++;
        // when the item is in the right order, stop
        if (!less(k, j)) break;
        // otherwise, exchange
        exch(k, j);
        k = j;
    }
}
```

# Delete the Maximum in a heap

- Exchange root with node at end, then sink it down.
- **Cost**: At most `2 lgN` compares.

The max `T` is swapped with the last node `E`, then removed. `E` now sits at the root and sinks
down until the heap order is restored:

**Before delMax (swap root with last node):**

<div class="mermaid">
graph TD
    bT["T (1)"]:::v --> bS["S (2)"]
    bT --> bR["R (3)"]
    bS --> bN["N (4)"]
    bS --> bG["G (5)"]
    bR --> bO["O (6)"]
    bR --> bA["A (7)"]
    bN --> bH["H (8)"]
    bN --> bE["E (9)"]:::w
    classDef v fill:#ffd54f,stroke:#c62828,stroke-width:2px
    classDef w fill:#90caf9,stroke:#1565c0,stroke-width:2px
</div>

**After removing T and sinking E:**

<div class="mermaid">
graph TD
    aS["S (1)"] --> aN["N (2)"]
    aS --> aR["R (3)"]
    aN --> aH["H (4)"]
    aN --> aG["G (5)"]
    aR --> aO["O (6)"]
    aR --> aA["A (7)"]
    aH --> aE["E (8)"]:::w
    classDef w fill:#90caf9,stroke:#1565c0,stroke-width:2px
    linkStyle 0,2,6 stroke:#1565c0,stroke-width:3px
</div>

```java
public Key delMax() {
    Key max = pq[1];
    exch(1, N--);
    sink(1);
    // prevent lotering
    pq[N+1] = null;
    return max;
}
```

# To be continued

> Part 2 [Binary Heap and Heapsort Summary - Part 2 - Heapsort]({% post_url 2018-06-16-binary-heap-heapsort-summary-part-2 %})
