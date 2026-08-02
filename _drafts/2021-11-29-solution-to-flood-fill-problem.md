---
layout: post
title: "Solution to Flood Fill problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [Flood Fill](https://leetcode.com/problems/flood-fill/)

An image is represented by an `m x n` integer grid `image` where `image[i][j]` represents the pixel
value of the image.

You are also given three integers `sr`, `sc`, and `newColor`. You should perform a **flood fill** on
the image starting from the pixel `image[sr][sc]`.

To perform a **flood fill**, consider the starting pixel, plus any pixels connected
**4-directionally** to the starting pixel of the same color as the starting pixel, plus any pixels
connected **4-directionally** to those pixels (also with the same color), and so on. Replace the
color of all of the aforementioned pixels with `newColor`.

Return *the modified image after performing the flood fill*.

**Example 1**

Starting from pixel `(sr, sc) = (1, 1)`, every pixel 4-directionally connected to it that shares the
same starting color (`1`) is repainted to `2`. The bottom-right `1` is left untouched because it is
not connected to the starting pixel through same-colored pixels. The starting pixel is outlined in
red.

<div style="display:flex;align-items:center;gap:24px;flex-wrap:wrap">
<figure style="margin:0;text-align:center">
<figcaption><strong>Input</strong></figcaption>
<svg width="184" height="184" viewBox="0 0 184 184" xmlns="http://www.w3.org/2000/svg" font-family="sans-serif" font-size="24">
  <g stroke="#333" stroke-width="1">
    <rect x="2"   y="2"   width="60" height="60" fill="#cfe8ff"/>
    <rect x="62"  y="2"   width="60" height="60" fill="#cfe8ff"/>
    <rect x="122" y="2"   width="60" height="60" fill="#cfe8ff"/>
    <rect x="2"   y="62"  width="60" height="60" fill="#cfe8ff"/>
    <rect x="122" y="62"  width="60" height="60" fill="#ffffff"/>
    <rect x="2"   y="122" width="60" height="60" fill="#cfe8ff"/>
    <rect x="62"  y="122" width="60" height="60" fill="#ffffff"/>
    <rect x="122" y="122" width="60" height="60" fill="#cfe8ff"/>
  </g>
  <rect x="62" y="62" width="60" height="60" fill="#cfe8ff" stroke="#c62828" stroke-width="3"/>
  <g fill="#333" text-anchor="middle">
    <text x="32"  y="40">1</text><text x="92"  y="40">1</text><text x="152" y="40">1</text>
    <text x="32"  y="100">1</text><text x="92"  y="100">1</text><text x="152" y="100">0</text>
    <text x="32"  y="160">1</text><text x="92"  y="160">0</text><text x="152" y="160">1</text>
  </g>
</svg>
</figure>
<div style="font-size:28px">&rarr;</div>
<figure style="margin:0;text-align:center">
<figcaption><strong>Output</strong></figcaption>
<svg width="184" height="184" viewBox="0 0 184 184" xmlns="http://www.w3.org/2000/svg" font-family="sans-serif" font-size="24">
  <g stroke="#333" stroke-width="1">
    <rect x="2"   y="2"   width="60" height="60" fill="#ffe082"/>
    <rect x="62"  y="2"   width="60" height="60" fill="#ffe082"/>
    <rect x="122" y="2"   width="60" height="60" fill="#ffe082"/>
    <rect x="2"   y="62"  width="60" height="60" fill="#ffe082"/>
    <rect x="62"  y="62"  width="60" height="60" fill="#ffe082"/>
    <rect x="122" y="62"  width="60" height="60" fill="#ffffff"/>
    <rect x="2"   y="122" width="60" height="60" fill="#ffe082"/>
    <rect x="62"  y="122" width="60" height="60" fill="#ffffff"/>
    <rect x="122" y="122" width="60" height="60" fill="#cfe8ff"/>
  </g>
  <g fill="#333" text-anchor="middle">
    <text x="32"  y="40">2</text><text x="92"  y="40">2</text><text x="152" y="40">2</text>
    <text x="32"  y="100">2</text><text x="92"  y="100">2</text><text x="152" y="100">0</text>
    <text x="32"  y="160">2</text><text x="92"  y="160">0</text><text x="152" y="160">1</text>
  </g>
</svg>
</figure>
</div>

```
Input: image = [[1,1,1],[1,1,0],[1,0,1]], sr = 1, sc = 1, newColor = 2
Output: [[2,2,2],[2,2,0],[2,0,1]]
Explanation: From the center of the image with position (sr, sc) = (1, 1) (i.e., the red pixel), all pixels connected by a path of the same color as the starting pixel (i.e., the blue pixels) are colored with the new color.
Note the bottom corner is not colored 2, because it is not 4-directionally connected to the starting pixel.
```

**Example 2**
```
Input: image = [[0,0,0],[0,0,0]], sr = 0, sc = 0, newColor = 2
Output: [[2,2,2],[2,2,2]]
```

**Constraints**
```
m == image.length
n == image[i].length
1 <= m, n <= 50
0 <= image[i][j], newColor < 216
0 <= sr < m
0 <= sc < n
```

<!-- more -->

**Solution**

Simply perform BFS from the starting point. Here is the sample working code in **C#**.

```csharp
public int[][] FloodFill(int[][] image, int sr, int sc, int newColor)
{
    // the starting point's color
    var sColor = image[sr][sc];

    // construct the array of visited points
    var visited = new bool[image.Length][];
    for (var r = 0; r < visited.Length; r++)
    {
        for (var c = 0; c < image[r].Length; c++)
        {
            visited[r] = new bool[image[r].Length];
        }
    }

    // bfs algorithm
    var q = new Queue<(int, int)>();
    q.Enqueue((sr, sc));
    while (q.Any())
    {
        var (r, c) = q.Dequeue();
        if (visited[r][c])
            continue;

        visited[r][c] = true;
        image[r][c] = newColor;

        // go up
        if (r != 0 && image[r - 1][c] == sColor)
            q.Enqueue((r - 1, c));
        // go down
        if (r != image.Length - 1 && image[r + 1][c] == sColor)
            q.Enqueue((r + 1, c));
        // go left
        if (c != 0 && image[r][c - 1] == sColor)
            q.Enqueue((r, c - 1));
        // go right
        if (c != image[r].Length - 1 && image[r][c + 1] == sColor)
            q.Enqueue((r, c + 1));
    }

    return image;
}
```
