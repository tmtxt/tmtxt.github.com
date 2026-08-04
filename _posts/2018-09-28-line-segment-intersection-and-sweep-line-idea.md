---
layout: post
title: "Line segment intersection and Sweep-line idea"
description: ""
categories: [algorithm]
mermaid: true
---

> just a blog post for summarizing my algorithm learning course.

# 1. Line segment intersection problem

Given `N` horizontal and vertical line segments, find all the points where they intersect, given that all x- and y-coordinates (of every endpoint) are distinct

**Quadratic algorithm**: check every pair of segments for intersection - O(N<sup>2</sup>).

<svg viewBox="0 0 760 280" width="100%" role="img" aria-label="Orthogonal line segments with intersection points highlighted" xmlns="http://www.w3.org/2000/svg" style="font-family: Verdana, sans-serif;">
  <line x1="60" y1="220" x2="700" y2="220" stroke="#1d1d1d" stroke-width="4"/>
  <line x1="120" y1="170" x2="560" y2="170" stroke="#1d1d1d" stroke-width="4"/>
  <line x1="220" y1="120" x2="650" y2="120" stroke="#1d1d1d" stroke-width="4"/>

  <line x1="260" y1="60" x2="260" y2="245" stroke="#0b5394" stroke-width="4"/>
  <line x1="470" y1="35" x2="470" y2="245" stroke="#0b5394" stroke-width="4"/>

  <circle cx="260" cy="220" r="7" fill="#cc0000"/>
  <circle cx="470" cy="220" r="7" fill="#cc0000"/>
  <circle cx="260" cy="170" r="7" fill="#cc0000"/>
  <circle cx="470" cy="170" r="7" fill="#cc0000"/>
  <circle cx="260" cy="120" r="7" fill="#cc0000"/>
  <circle cx="470" cy="120" r="7" fill="#cc0000"/>

  <text x="68" y="244" font-size="14" fill="#333">horizontal segments</text>
  <text x="500" y="48" font-size="14" fill="#0b5394">vertical segments</text>
  <text x="520" y="260" font-size="14" fill="#cc0000">intersection points</text>
</svg>

# 2. Sweep-line idea

A vertical segment can only ever intersect a horizontal one, so the search really comes down to: for every vertical segment, which horizontal segments cross it?

Instead of comparing every pair, sweep an imaginary vertical line across the plane from left to
right. The x-coordinate of every endpoint becomes an **event**, and events are processed in
x-order:

- **h-segment, left endpoint**: insert its y-coordinate into a BST - the segment is now "active"
  (it currently crosses the sweep line).
- **h-segment, right endpoint**: remove its y-coordinate from the BST - the segment is no longer
  active.
- **v-segment**: since a vertical segment lives entirely at one x-coordinate, do a **1d range
  search** in the BST for the segment's `[y_lo, y_hi]` interval - every active h-segment whose
  y-coordinate falls in that range crosses the vertical segment right here.

In other words, the BST always holds the y-coordinates of the h-segments the sweep line is
currently passing through, and a v-segment turns into exactly the 1d range search from the
[previous post]({% post_url 2018-09-27-1d-range-search %}).

# 3. Worked example

Take four h-segments and one v-segment (all x-coordinates distinct, as the nondegeneracy
assumption requires):

- `0`: y = 0, x from 0 to 11
- `1`: y = 1, x from 1 to 8
- `2`: y = 2, x from 2 to 4
- `3`: y = 3, x from 3 to 9
- `4` (vertical): x = 6, y from 0.5 to 2.5

<svg width="400" height="230" viewBox="0 0 400 230" xmlns="http://www.w3.org/2000/svg" style="font-family: sans-serif; font-size: 11px;">
  <rect x="208" y="10" width="8" height="210" fill="#f4cccc"/>
  <line x1="212" y1="10" x2="212" y2="220" stroke="#cc0000" stroke-width="1.5" stroke-dasharray="4 3"/>

  <line x1="20" y1="210" x2="372" y2="210" stroke="#000" stroke-width="2"/>
  <circle cx="20" cy="210" r="3"/>
  <circle cx="372" cy="210" r="3"/>
  <text x="6" y="214">0</text>

  <line x1="52" y1="152" x2="276" y2="152" stroke="#000" stroke-width="2"/>
  <circle cx="52" cy="152" r="3"/>
  <circle cx="276" cy="152" r="3"/>
  <text x="38" y="156">1</text>

  <line x1="84" y1="94" x2="148" y2="94" stroke="#000" stroke-width="2"/>
  <circle cx="84" cy="94" r="3"/>
  <circle cx="148" cy="94" r="3"/>
  <text x="70" y="98">2</text>

  <line x1="116" y1="36" x2="308" y2="36" stroke="#000" stroke-width="2"/>
  <circle cx="116" cy="36" r="3"/>
  <circle cx="308" cy="36" r="3"/>
  <text x="102" y="40">3</text>

  <line x1="212" y1="65" x2="212" y2="181" stroke="#000" stroke-width="2"/>
  <circle cx="212" cy="65" r="3"/>
  <circle cx="212" cy="181" r="3"/>
  <text x="218" y="58">4</text>

  <circle cx="212" cy="152" r="6" fill="none" stroke="#cc0000" stroke-width="2"/>
  <text x="184" y="222" fill="#cc0000">sweep line, x = 6</text>
</svg>

Processing events left to right:

| x   | event                                                 | BST after event |
| --- | ----------------------------------------------------- | --------------- |
| 0   | insert `0` (left of `0`)                              | `{0}`           |
| 1   | insert `1` (left of `1`)                              | `{0, 1}`        |
| 2   | insert `2` (left of `2`)                              | `{0, 1, 2}`     |
| 3   | insert `3` (left of `3`)                              | `{0, 1, 2, 3}`  |
| 4   | delete `2` (right of `2`)                             | `{0, 1, 3}`     |
| 6   | range search `[0.5, 2.5]` on `{0, 1, 3}` -> match `1` | `{0, 1, 3}`     |
| 8   | delete `1` (right of `1`)                             | `{0, 3}`        |
| 9   | delete `3` (right of `3`)                             | `{0}`           |
| 11  | delete `0` (right of `0`)                             | `{}`            |
{: .table }

By the time the sweep line reaches the v-segment at `x = 6`, segment `2` has already been deleted
(its right endpoint was at `x = 4`), so the BST only holds `{0, 1, 3}`:

<div class="mermaid">
graph TD
  N1(("1")) --> N0(("0"))
  N1 --> N3(("3"))

classDef inRange fill:#b6d7a8,stroke:#38761d,stroke-width:2px;
classDef compareOnly fill:#f4cccc,stroke:#cc0000,stroke-width:2px;

class N1 inRange
class N0,N3 compareOnly

</div>

The range search for `[0.5, 2.5]` only matches `1` (green): `0` is below the range and `3` is
above it, so segment `4` intersects only segment `1`, at the point `(6, 1)`.

# 4. Sweep-line characteristics

The sweep-line algorithm takes time proportional to `N log N + R` to find all `R`
intersections among `N` orthogonal line segments.

| action                                              | cost          |
| --------------------------------------------------- | ------------- |
| put x-coordinates on a min priority queue (or sort) | `N log N`     |
| insert y-coordinates into the BST                   | `N log N`     |
| delete y-coordinates from the BST                   | `N log N`     |
| range searches in the BST                           | `N log N + R` |
{: .table }

The sweep line reduces 2d orthogonal line segment intersection search to
[1d range search]({% post_url 2018-09-27-1d-range-search %}) - the same `O(log N)` insert/delete
and `O(R + log N)` range search from a balanced BST carry straight over, just applied once per
event instead of once overall.
