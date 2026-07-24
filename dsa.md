---
layout: page
title: "Collections"
description: ""
group: project
mermaid: true
---

My DSA learning, organized by topics and charts with link to corresponding posts

# Sorting

<div class="mermaid">
graph TD
    Sorting --> ElementarySort[Elementary Sort]
    Sorting --> MergeSort[Merge Sort]
    Sorting --> QuickSort[Quick Sort]
    Sorting --> HeapSort[Heap Sort - Priority Queue]
    ElementarySort --> SelectionSort[Selection Sort]
    ElementarySort --> InsertionSort[Insertion Sort]
    ElementarySort --> ShellSort[Shell Sort]
		MergeSort --> BottomUpMergeSort[Bottom Up Merge Sort]
</div>

Elementary Sort
- [Elementary Sorts Summary]({% post_url 2018-05-19-elementary-sorts-summary %})
  - Selection Sort
	- Insertion Sort
	- Shell Sort

Merge Sort
- [Merge Sort Summary]({% post_url 2018-05-23-merge-sort-summary %})
  - Merge Sort basic
  - Bottom up Merge Sort
- [Merge Sort Related Questions]({% post_url 2018-05-23-summary-merge-sort-related-questions %})
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

# Algorithm summary

Dynamic Connectivity & Union Find
- [Dynamic Connectivity & Union Find - Summary]({% post_url 2022-09-03-dynamic-connectivity-union-find-summary %})
- [Dynamic Connectivity & Union Find - Related Questions]({% post_url 2018-05-01-union-find-summary-part-5 %})

Stacks & Queues
- [Stacks and Queues summary]({% post_url 2018-05-15-stacks-and-queues-summary %})

# Algorithm exercises

- [3-sum problem]({% post_url 2018-05-05-solution-to-3-sum-problem %})
- [Sorting with 1MB RAM computer]({% post_url 2018-05-05-solutions-to-the-sorting-with-1mb-ram-computer-problem %}})
- [Egg Drop problem]({% post_url 2018-05-06-solutions-to-egg-drop-problem %})
- [Find (i->j) sequence by sum]({% post_url 2018-05-12-solution-to-find-i-j-sequence-by-sum %})