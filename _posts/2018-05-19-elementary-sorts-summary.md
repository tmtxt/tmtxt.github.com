---
layout: post
title: "Elementary Sorts Summary"
description: ""
categories: [algorithm]
mermaid: true
thumbnail:
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Although
> some of them were already taught in the University, it's still good to summarise here

# 1. Selection Sort

- In iteration `i`, find index min of smallest remaining entry.
- Swap `a[i]` and `a[min]`

```typescript
function selectionSort(a: number[]): void {
  const n = a.length;
  for (let i = 0; i < n; i++) {

    let min: number = i;
    for (let j = i + 1; j < n; j++) {
      if (a[j] < a[min]) {
        min = j;
      }
    }

    swap(a, i, min); // swap the 2 items
  }
}
```

<!-- more -->

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

- Like the reversed way of Selection Sort
- In iteration `i`, swap `a[i]` with each larger entry to its left.

```typescript
function insertionSort(a: number[]): void {
  const n = a.length;
  for (let i = 0; i < n; i++) {
    for (let j = i; j > 0; j--) {
      if (a[j] < a[j-1]) {
        swap(a, j, j-1);
      } else {
        break;
      }
    }
  }
}
```

- **Best case**: If the array is in ascending order, insertion sort makes `N - 1` compares and `0`
exchanges.
  - Eg: A E E L M O P R S T X
- **Worst case**: If the array is in descending order (and no duplicates), insertion
  sort makes ~ **1/2 N<sup>2</sup>** compares and ~ **1/2 N<sup>2</sup>** exchanges.
  - Eg: X T S R P O M L E E A
- Still a bit better than **Selection Sort**

# 3. Shell Sort

## 3.1 h-Sorted Array

An **h-sorted** array is h interleaved sorted sub-sequences. For example, in a **4-sorted**
array of 12 elements, every 4th element forms its own independent sorted sub-sequence:

| Index | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 |
|-------|---|---|---|---|---|---|---|---|---|---|----|----|
| Group | A | B | C | D | A | B | C | D | A | B | C  | D  |
{: .table }

<div class="mermaid">
flowchart LR
    subgraph A["Group A (indices 0, 4, 8)"]
        direction LR
        A0["a[0]"] --> A4["a[4]"] --> A8["a[8]"]
    end
    subgraph B["Group B (indices 1, 5, 9)"]
        direction LR
        B1["a[1]"] --> B5["a[5]"] --> B9["a[9]"]
    end
    subgraph C["Group C (indices 2, 6, 10)"]
        direction LR
        C2["a[2]"] --> C6["a[6]"] --> C10["a[10]"]
    end
    subgraph D["Group D (indices 3, 7, 11)"]
        direction LR
        D3["a[3]"] --> D7["a[7]"] --> D11["a[11]"]
    end
</div>

Each arrow means "less than or equal to", so within group A, `a[0] <= a[4] <= a[8]`, and the same
rule applies to groups B, C and D. The whole array is **4-sorted** once all four interleaved
sub-sequences are independently sorted this way.

- To **h-sort** an array, use insertion sort with stride length **h**. Which means, for each
  iteration `i`, instead of going back by one step each, go back `h` steps. Here is an example of
  **3-sorting** the array `[8, 5, 3, 9, 1, 7, 4, 6, 2]`.

With `h = 3`, the array splits into 3 groups: indices `{0, 3, 6}`, `{1, 4, 7}` and `{2, 5, 8}`.
Each group is sorted independently, as if running plain insertion sort on just those elements:

| Group       | Before sort | After sort |
|-------------|-------------|------------|
| `{0, 3, 6}` | 8, 9, 4     | 4, 8, 9    |
| `{1, 4, 7}` | 5, 1, 6     | 1, 5, 6    |
| `{2, 5, 8}` | 3, 7, 2     | 2, 3, 7    |
{: .table }

Putting the sorted groups back into their original positions gives the **3-sorted** array:

| Index         | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---------------|---|---|---|---|---|---|---|---|---|
| Before 3-sort | 8 | 5 | 3 | 9 | 1 | 7 | 4 | 6 | 2 |
| After 3-sort  | 4 | 1 | 2 | 8 | 5 | 3 | 9 | 6 | 7 |
{: .table }

Notice how each group is now internally sorted (e.g. `4 <= 8 <= 9` for `{0, 3, 6}`), even though
the array as a whole is still far from fully sorted.

## 3.2 What is Shell Sort?

- Move entries more than one position at a time by **h-sorting** the array
- **h-sort** array for decreasing sequence of values of h until we reach a **1-sorted** array
- For example, 13-sort the array, 4-sort the result and then 1-sort the array to get the final
  sorted array

<div class="mermaid">
flowchart LR
    Start(["Unsorted array"]) --> H13["13-sort"] --> H4["4-sort"] --> H1["1-sort<br/>(regular insertion sort)"] --> Done(["Fully sorted array"])
</div>

Each step takes the array produced by the previous step and makes it `h`-sorted for a smaller
value of `h`. By the time `h` reaches `1`, the array only needs a regular insertion sort to
become fully sorted - and because it is already "almost sorted" by then, that final pass is very
fast.

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
