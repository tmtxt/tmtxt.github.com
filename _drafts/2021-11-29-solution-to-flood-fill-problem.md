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

![Flood Fill](/files/2021-11-29-solution-to-flood-fill-problem/flood1-grid.jpg)

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
