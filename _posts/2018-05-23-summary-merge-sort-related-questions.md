---
layout: post
title: "Merge Sort Related Questions"
description: ""
categories: [algorithm]
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Here are
> some questions related to merge sort.

# Merging with smaller auxiliary array

**Question**

Suppose that the subarray `a[0]` to `a[n-1]` is sorted and the subarray
`a[n]` to `a[2*n-1]` is sorted. How can you merge the two subarrays so
that `a[0]` to `a[2*n-1]` is sorted using an auxiliary array of length `n`
(instead of `2n`)?

**Answer**

Instead of copying the whole array into the auxiliary array, copy only the left half to that
auxiliary array (`a[0]` to `a[n-1]`). At this time, the first half of the original array is free to
overwrite any value. Apply merge sort and write the result back to the original array, from the
first position. You will always have enough space in the original array to do that.

# Counting inversions

**Question**

An inversion in an array `a[]` is a pair of entries `a[i]` and `a[j]` such that `i<j` but
`a[i]>a[j]`. Given an array, design a linearithmic algorithm to count the number of inversions.

<!-- more -->

**Answer**

> Actually, I couldn't think of a solution for this even with the hint. Here is the solution that I
> found on the internet but it is also interesting to take a look :D I would give 1 like for the one
> that could think of the solution for this if I met him :LOL: It took me about 10 min to
> understand...

The basic idea is to use merge sort and count while merging the 2 arrays. In merge process, let `i` is
used for indexing left sub-array and `j` for right sub-array. At any step in `merge()`, if `a[i]` is
greater than `a[j]`, then there are `(mid – i)` inversions. because left and right subarrays are sorted,
so all the remaining elements in left-subarray (`a[i+1], a[i+2] … a[mid]`) will be greater than
`a[j]`. Here is the sample implementation in Java

```java
class Test {
    // This method sorts the input array and returns
    // the number of inversions in the array
    static int mergeSort(int arr[], int array_size) {
        int temp[] = new int[array_size];
        return _mergeSort(arr, temp, 0, array_size - 1);
    }

    // An auxiliary recursive method that sorts the input array and
    // returns the number of inversions in the array.
    static int _mergeSort(int arr[], int temp[], int left, int right) {
        int mid, inv_count = 0;
        if (right > left) {
            // Divide the array into two parts and call _mergeSort()
            // for each of the parts
            mid = (right + left) / 2;

            // Inversion count will be sum of inversions in left-part, right-part
            // and number of inversions in merging */
            inv_count = _mergeSort(arr, temp, left, mid);
            inv_count += _mergeSort(arr, temp, mid + 1, right);

            // Merge the two parts
            inv_count += merge(arr, temp, left, mid + 1, right);
        }
        return inv_count;
    }

    // This method merges two sorted arrays and returns inversion count in
    // the arrays.
    static int merge(int arr[], int temp[], int left, int mid, int right) {
        int i, j, k;
        int inv_count = 0;

        i = left; // i is index for left subarray
        j = mid;  // j is index for right subarray
        k = left; // k is index for resultant merged subarray
        while ((i <= mid - 1) && (j <= right)) {
            if (arr[i] <= arr[j]) {
                temp[k++] = arr[i++];
            } else {
                temp[k++] = arr[j++];

                // THE MAGIC HAPPENS HERE
                // this is tricky -- see above explanation for merge()
                inv_count = inv_count + (mid - i);
            }
        }

        // Copy the remaining elements of left subarray
        // (if there are any) to temp
        while (i <= mid - 1)
            temp[k++] = arr[i++];

        // Copy the remaining elements of right subarray
        // (if there are any) to temp
        while (j <= right)
            temp[k++] = arr[j++];

        // Copy back the merged elements to original array
        for (i = left; i <= right; i++)
            arr[i] = temp[i];

        return inv_count;
    }
}
```
