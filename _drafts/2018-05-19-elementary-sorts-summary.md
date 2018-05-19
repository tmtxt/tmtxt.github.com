---
layout: post
title: "Elementary Sorts Summary"
description: ""
categories: [algorithm]
thumbnail:
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Although
> this was already taught in the University, it's still good to summarise here

# 1. Selection Sort

* In iteration `i`, find index min of smallest remaining entry.
* Swap `a[i]` and `a[min]`

![Selection Sort Gif](/files/2018-05-19-elementary-sorts-summary/selection-sort.gif)

```java
class Selection {
    public static void sort(Comparable[] a) {
        int N = a.length;
        for (int i = 0; i < N; i++) {
            int min = i;
            for (int j = i + 1; j < N; j++)
                if (less(a[j], a[min]))
                    min = j;
            swap(a, i, min); // swap the 2 items
        }
    }
}
```

![Animation](/files/2018-05-19-elementary-sorts-summary/selection-sort_c041bf.gif)

<math xmlns="http://www.w3.org/1998/Math/MathML">
  <ms>Complexity:</ms>
  <mspace />
  <mi>O(</mi>
  <msup>
    <mi>N</mi>
    <mn>2</mn>
  </msup>
  <mi>/2)</mi>
  <mi>~</mi>
  <mi>O(</mi>
  <msup>
    <mi>N</mi>
    <mn>2</mn>
  </msup>
  <mi>)</mi>
</math>

# 2. Insertion Sort

* Like the reversed way of Selection Sort
* In iteration i, swap a[i] with each larger entry to its left.

```java
class Insertion {
    public static void sort(Comparable[] a) {
        int N = a.length;
        for (int i = 0; i < N; i++)
            for (int j = i; j > 0; j--)
                if (less(a[j], a[j - 1]))
                    swap(a, j, j - 1);
                else break;
    }
}
```

![Animation](/files/2018-05-19-elementary-sorts-summary/insertion-sort_e8e408.gif)

- **Best case**: If the array is in ascending order, insertion sort makes `N - 1` compares and `0`
exchanges.
  - Eg: A E E L M O P R S T X
- **Worst case**: If the array is in descending order (and no duplicates), insertion
  sort makes ~ **1/2 N<sup>2</sup>** compares and ~ **1/2 N<sup>2</sup>** exchanges.
  - Eg: X T S R P O M L E E A
- Still a bit better than **Selection Sort**

# 3. Shell Sort

