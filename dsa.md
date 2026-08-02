---
layout: page
title: "Data Structures & Algorithms"
description: ""
group: project
mermaid: true
---

> My DSA learning, organized by topics and charts with link to corresponding posts

_still in progress..._

# 1. Sorting

<div class="mermaid">
graph TD
    Sorting --> ElementarySort[Elementary Sort]
    Sorting --> MergeSort[Merge Sort]
    Sorting --> QuickSort[Quick Sort]
    Sorting --> HeapSort["Heap Sort<br/>Priority Queue"]
    ElementarySort --> SelectionSort[Selection Sort]
    ElementarySort --> InsertionSort[Insertion Sort]
    ElementarySort --> ShellSort[Shell Sort]
		MergeSort --> BottomUpMergeSort[Bottom Up Merge Sort]
</div>

Elementary Sort

- [Elementary Sorts Summary]({% post_url 2018-05-19-elementary-sorts-summary %}), including
  - Selection Sort
  - Insertion Sort
  - Shell Sort
- Shuffling - TBA
- Convex Hull - TBA

Merge Sort

- [Merge Sort Summary]({% post_url 2018-05-23-merge-sort-summary %}), including
  - Merge Sort basic
  - Bottom up Merge Sort
- [Merge Sort Related Questions]({% post_url 2018-05-23-summary-merge-sort-related-questions %}), including
  - Merging with smaller auxiliary array
  - Counting inversions
- [Merge Sort and the Interview]({% post_url 2024-11-30-merge-sort-and-the-interview %})

Quick Sort

- [Quick Sort summary - Part 1 - Basic Implementation]({% post_url 2018-05-29-quick-sort-summary-part-1 %})
- [Quick Sort summary - Part 2 - Selection Problem]({% post_url 2018-06-02-quick-sort-summary-part-2 %})
- [Quick Sort summary - Part 3 - 3-way Partitioning]({% post_url 2018-06-02-quick-sort-summary-part-3 %})
- Quick Sort summary - Part 4 - Related Questions - Still in my draft folder

Heap Sort

- [Binary Heap and Heapsort Summary - Part 1 - Binary Heap]({% post_url 2018-06-07-binary-heap-heapsort-summary-part-1 %})
- [Binary Heap and Heapsort Summary - Part 2 - Heapsort]({% post_url 2018-06-16-binary-heap-heapsort-summary-part-2 %})
- [Priority Queues - Related Questions]({% post_url 2021-10-22-priority-queue-related-questions %})

# 2. Data Structure

## Symbol Table

<div class="mermaid">
graph TD
    SymbolTable[Symbol Table]
    SymbolTable --> BST[Binary Search Tree - BST]
    SymbolTable --> TwoThree[2-3 Search Trees]
    TwoThree --> RedBlack[Red-Black BST]
    BST --> RedBlack
    TwoThree --> BTrees[B-Trees]
</div>

- [Symbol Tables and Binary Search Trees summary]({% post_url 2018-09-23-symbol-tables-and-binary-search-trees-summary %})
- [2-3 search trees]({% post_url 2018-09-24-2-3-search-trees %})

## Others

- [Stacks and Queues summary]({% post_url 2018-05-15-stacks-and-queues-summary %})

# 3. Other DSA topics

Dynamic Connectivity & Union Find

- [Dynamic Connectivity & Union Find - Summary]({% post_url 2022-09-03-dynamic-connectivity-union-find-summary %})
- [Dynamic Connectivity & Union Find - Related Questions]({% post_url 2018-05-01-union-find-summary-part-5 %})

# 4. Exercises by topics

## General

- [Sorting with 1MB RAM computer]({% post_url 2018-05-05-solutions-to-the-sorting-with-1mb-ram-computer-problem %}})
- [Egg Drop problem]({% post_url 2018-05-06-solutions-to-egg-drop-problem %})
- [Find (i->j) sequence by sum]({% post_url 2018-05-12-solution-to-find-i-j-sequence-by-sum %})

## Stack

- Longest Valid Parentheses solution - Still in draft folder, the solution there is outdated

## 2 pointers

- Two Sum with Sorted array problem - in draft folder
- [3-sum problem]({% post_url 2018-05-05-solution-to-3-sum-problem %})
