---
layout: post
title: "Sample code for Percolation problem"
description: ""
categories: [algorithm]
thumbnail: /files/2018-04-30-union-find-summary/img1.png
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course.

Source here:
[https://introcs.cs.princeton.edu/java/24percolation/](https://introcs.cs.princeton.edu/java/24percolation/),
put the whole description here so that later I can easily search and review myself.

The **Percolation** problem can be described like this

**Percolation**: Given a composite systems comprised of randomly distributed insulating and metallic
materials: what fraction of the materials need to be metallic so that the composite system is an
electrical conductor? Given a porous landscape with water on the surface (or oil below), under what
conditions will the water be able to drain through to the bottom (or the oil to gush through to the
surface)? Scientists have defined an abstract process known as percolation to model such situations.

**The model** We model a percolation system using an n-by-n grid of sites. Each site is either open or
blocked. A full site is an open site that can be connected to an open site in the top row via a
chain of neighboring (left, right, up, down) open sites. We say the system percolates if there is a
full site in the bottom row. In other words, a system percolates if we fill all open sites connected
to the top row and that process fills some open site on the bottom row. (For the insulating/metallic
materials example, the open sites correspond to metallic materials, so that a system that percolates
has a metallic path from top to bottom, with full sites conducting. For the porous substance
example, the open sites correspond to empty space through which water might flow, so that a system
that percolates lets water fill open sites, flowing from top to bottom.)

<!-- more -->

![Percolates Yes](/files/2018-05-08-sample-code-for-percolation-problem/percolates-yes.png)

![Percolates No](/files/2018-05-08-sample-code-for-percolation-problem/percolates-no.png)

**The problem**: In a famous scientific problem, researchers are interested in the following question:
if sites are independently set to be open with probability p (and therefore blocked with probability
1 − p), what is the probability that the system percolates? When p equals 0, the system does not
percolate; when p equals 1, the system percolates. The plots below show the site vacancy probability
p versus the percolation probability for 20-by-20 random grid (left) and 100-by-100 random grid
(right).

![Percolation Threshold 20](/files/2018-05-08-sample-code-for-percolation-problem/percolation-threshold20.png)

![Percolation Threshold 100](/files/2018-05-08-sample-code-for-percolation-problem/percolation-threshold100.png)

When n is sufficiently large, there is a threshold value p* such that when p < p* a random n-by-n
grid almost never percolates, and when p > p\*, a random n-by-n grid almost always percolates. No
mathematical solution for determining the percolation threshold p\* has yet been derived. Your task
is to write a computer program to estimate p\*.

Here is the sample code
[https://github.com/tmtxt/percolation-solution](https://github.com/tmtxt/percolation-solution)

To run it, install **Intellij** and open the project. You need to configure class path for
`algs4.jar` (download [here](https://algs4.cs.princeton.edu/code/algs4.jar))

The source code contains 2 Java classes

- `Percolation`: the Percolation data structure, for storing data of all sites of 1 system and check
  whether that system percolates or not. It uses Weighted Quick Union for all the methods.
  The Percolation data structure has these public methods

|Method|Description|
|----|----|
|`public Percolation(int n)`|create n-by-n grid, with all sites blocked|
|`public void open(int row, int col)`|open site (row, col) if it is not open already|
|`public boolean isOpen(int row, int col)`|is site (row, col) open?|
|`public boolean isFull(int row, int col)`|is site (row, col) full?|
|`public int numberOfOpenSites()`|number of open sites|
|`public boolean percolates()`|does the system percolate?|
{: .table }

- `PercolationStats`: class for running a series of `trials` computational experiments on a system
  with size `n*n`. Simply initialize `trials` and `n` using its constructor
