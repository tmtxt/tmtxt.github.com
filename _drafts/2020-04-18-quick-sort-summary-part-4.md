---
layout: post
title: "Quick Sort summary - Part 4 - Related Questions"
description: ""
categories: [algorithm]
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Here are
> some questions related to Quick Sort

# Nuts and bolts

**Question**: A disorganized carpenter has a mixed pile of `n` nuts and `n` bolts. The goal is to
find the corresponding pairs of nuts and bolts. Each nut fits exactly one bolt and each bolt fits
exactly one nut. By fitting a nut and a bolt together, the carpenter can see which one is bigger
(but the carpenter cannot compare two nuts or two bolts directly). Design an algorithm for the
problem that uses `NlogN` compares (probabilistically).

**Solution**:

Pick one nut, loop the list of bolt, find the matching bolt and also divide the bolts into 2
collections, smaller and larger. Pick the matched bolt, compare with all the remaining nuts to
divide them into 2 sets, smaller and larger. Repeat the process recursively.

# Selection in two sorted arrays

Given two sorted arrays `a[]` and `b[]`, of sizes n<sub>1</sub> and n<sub>2</sub> respectively,
design an algorithm to find the k<sup>th</sup> largest key. The order of growth of the worst case
running time of your algorithm should be *n*log*n*, where n=n<sub>1</sub>+n<sub>2</sub>

- Version 1: n<sub>1</sub>=n<sub>2</sub> and k=n/2
- Version 2: k=n/2
- Version 3: no restrictions
