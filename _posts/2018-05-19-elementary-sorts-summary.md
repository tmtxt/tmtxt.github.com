---
layout: post
title: "Elementary Sorts Summary"
description: ""
categories: [algorithm]
thumbnail:
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Although
> some of them were already taught in the University, it's still good to summarise here

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

<!-- more -->

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

## 3.1 h-Sorted Array

An **h-sorted** array is h interleaved sorted sub-sequences. Here is a **4-sorted** array

![4-sorted array](/files/2018-05-19-elementary-sorts-summary/h-sort.png)

- To **h-sort** an array, use insertion sort with stride length **h**. Which means, for each
  iteration `i`, instead of going back by one step each, go back `h` steps. Here is an example of
  **3-sorting** an array.

![Shell Sort](/files/2018-05-19-elementary-sorts-summary/h-sort3.png)

## 3.2 What is Shell Sort?

- Move entries more than one position at a time by **h-sorting** the array
- **h-sort** array for decreasing sequence of values of h until we reach a **1-sorted** array
- For example, 13-sort the array, 4-sort the result and then 1-sort the array to get the final
  sorted array

![Shell Sort](/files/2018-05-19-elementary-sorts-summary/h-sort2.png)

- Some increasing of `h` values to use
  - `3x + 1`: 1, 4, 13, 40, 121, 364, ...
  - `Sedgewick`: 1, 5, 19, 41, 109, 209, 505, 929, 2161, 3905, ...

## 3.3 Sample Code

```java
class Shell {
    public static void sort(Comparable[] a) {
        int N = a.length;
        int h = 1;

        // 1, 4, 13, 40, 121, 364, ...
        while (h < N / 3) h = 3 * h + 1;

        // repeat until we get a 1-sorted array
        while (h >= 1) {
            // h-sort the array using insertion sort but with stride length h
            for (int i = h; i < N; i++) {
                for (int j = i; j >= h && less(a[j], a[j - h]); j -= h)
                    swap(a, j, j - h);
            }
            h = h / 3;
        }
    }
}
```
