---
layout: post
title: "Solution to Max Area of Island problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [Max Area of Island](https://leetcode.com/problems/max-area-of-island/)

You are given an `m x n` binary matrix `grid`. An island is a group of `1`'s (representing land)
connected **4-directionally** (horizontal or vertical.) You may assume all four edges of the grid
are surrounded by water.

The **area** of an island is the number of cells with a value `1` in the island.

Return the *maximum* **area** *of an island* in `grid`. If there is no island, return `0`.

**Example 1**

The grid below has several islands. Land cells (`1`) are light blue, water (`0`) is white, and the
largest island - the one with area `6` (rows 3-5, columns 8-10) - is highlighted in orange.

<figure style="margin:0">
<svg width="368" height="228" viewBox="0 0 368 228" xmlns="http://www.w3.org/2000/svg">
  <rect x="2" y="2" width="364" height="224" fill="#ffffff"/>
  <g fill="#cfe8ff">
    <rect x="58"  y="2"   width="28" height="28"/><rect x="198" y="2"   width="28" height="28"/>
    <rect x="198" y="30"  width="28" height="28"/><rect x="226" y="30"  width="28" height="28"/><rect x="254" y="30"  width="28" height="28"/>
    <rect x="30"  y="58"  width="28" height="28"/><rect x="58"  y="58"  width="28" height="28"/><rect x="114" y="58"  width="28" height="28"/>
    <rect x="30"  y="86"  width="28" height="28"/><rect x="114" y="86"  width="28" height="28"/><rect x="142" y="86"  width="28" height="28"/>
    <rect x="30"  y="114" width="28" height="28"/><rect x="114" y="114" width="28" height="28"/><rect x="142" y="114" width="28" height="28"/>
    <rect x="198" y="170" width="28" height="28"/><rect x="226" y="170" width="28" height="28"/><rect x="254" y="170" width="28" height="28"/>
    <rect x="198" y="198" width="28" height="28"/><rect x="226" y="198" width="28" height="28"/>
  </g>
  <g fill="#ffb74d">
    <rect x="226" y="86"  width="28" height="28"/><rect x="282" y="86"  width="28" height="28"/>
    <rect x="226" y="114" width="28" height="28"/><rect x="254" y="114" width="28" height="28"/><rect x="282" y="114" width="28" height="28"/>
    <rect x="282" y="142" width="28" height="28"/>
  </g>
  <g stroke="#bbbbbb" stroke-width="1">
    <line x1="2"   y1="2" x2="2"   y2="226"/><line x1="30"  y1="2" x2="30"  y2="226"/><line x1="58"  y1="2" x2="58"  y2="226"/>
    <line x1="86"  y1="2" x2="86"  y2="226"/><line x1="114" y1="2" x2="114" y2="226"/><line x1="142" y1="2" x2="142" y2="226"/>
    <line x1="170" y1="2" x2="170" y2="226"/><line x1="198" y1="2" x2="198" y2="226"/><line x1="226" y1="2" x2="226" y2="226"/>
    <line x1="254" y1="2" x2="254" y2="226"/><line x1="282" y1="2" x2="282" y2="226"/><line x1="310" y1="2" x2="310" y2="226"/>
    <line x1="338" y1="2" x2="338" y2="226"/><line x1="366" y1="2" x2="366" y2="226"/>
    <line x1="2" y1="2"   x2="366" y2="2"/><line x1="2" y1="30"  x2="366" y2="30"/><line x1="2" y1="58"  x2="366" y2="58"/>
    <line x1="2" y1="86"  x2="366" y2="86"/><line x1="2" y1="114" x2="366" y2="114"/><line x1="2" y1="142" x2="366" y2="142"/>
    <line x1="2" y1="170" x2="366" y2="170"/><line x1="2" y1="198" x2="366" y2="198"/><line x1="2" y1="226" x2="366" y2="226"/>
  </g>
  <g font-family="sans-serif" font-size="13" fill="#555" text-anchor="middle">
    <text x="16" y="21">0</text><text x="44" y="21">0</text><text x="72" y="21">1</text><text x="100" y="21">0</text><text x="128" y="21">0</text><text x="156" y="21">0</text><text x="184" y="21">0</text><text x="212" y="21">1</text><text x="240" y="21">0</text><text x="268" y="21">0</text><text x="296" y="21">0</text><text x="324" y="21">0</text><text x="352" y="21">0</text>
    <text x="16" y="49">0</text><text x="44" y="49">0</text><text x="72" y="49">0</text><text x="100" y="49">0</text><text x="128" y="49">0</text><text x="156" y="49">0</text><text x="184" y="49">0</text><text x="212" y="49">1</text><text x="240" y="49">1</text><text x="268" y="49">1</text><text x="296" y="49">0</text><text x="324" y="49">0</text><text x="352" y="49">0</text>
    <text x="16" y="77">0</text><text x="44" y="77">1</text><text x="72" y="77">1</text><text x="100" y="77">0</text><text x="128" y="77">1</text><text x="156" y="77">0</text><text x="184" y="77">0</text><text x="212" y="77">0</text><text x="240" y="77">0</text><text x="268" y="77">0</text><text x="296" y="77">0</text><text x="324" y="77">0</text><text x="352" y="77">0</text>
    <text x="16" y="105">0</text><text x="44" y="105">1</text><text x="72" y="105">0</text><text x="100" y="105">0</text><text x="128" y="105">1</text><text x="156" y="105">1</text><text x="184" y="105">0</text><text x="212" y="105">0</text><text x="240" y="105">1</text><text x="268" y="105">0</text><text x="296" y="105">1</text><text x="324" y="105">0</text><text x="352" y="105">0</text>
    <text x="16" y="133">0</text><text x="44" y="133">1</text><text x="72" y="133">0</text><text x="100" y="133">0</text><text x="128" y="133">1</text><text x="156" y="133">1</text><text x="184" y="133">0</text><text x="212" y="133">0</text><text x="240" y="133">1</text><text x="268" y="133">1</text><text x="296" y="133">1</text><text x="324" y="133">0</text><text x="352" y="133">0</text>
    <text x="16" y="161">0</text><text x="44" y="161">0</text><text x="72" y="161">0</text><text x="100" y="161">0</text><text x="128" y="161">0</text><text x="156" y="161">0</text><text x="184" y="161">0</text><text x="212" y="161">0</text><text x="240" y="161">0</text><text x="268" y="161">0</text><text x="296" y="161">1</text><text x="324" y="161">0</text><text x="352" y="161">0</text>
    <text x="16" y="189">0</text><text x="44" y="189">0</text><text x="72" y="189">0</text><text x="100" y="189">0</text><text x="128" y="189">0</text><text x="156" y="189">0</text><text x="184" y="189">0</text><text x="212" y="189">1</text><text x="240" y="189">1</text><text x="268" y="189">1</text><text x="296" y="189">0</text><text x="324" y="189">0</text><text x="352" y="189">0</text>
    <text x="16" y="217">0</text><text x="44" y="217">0</text><text x="72" y="217">0</text><text x="100" y="217">0</text><text x="128" y="217">0</text><text x="156" y="217">0</text><text x="184" y="217">0</text><text x="212" y="217">1</text><text x="240" y="217">1</text><text x="268" y="217">0</text><text x="296" y="217">0</text><text x="324" y="217">0</text><text x="352" y="217">0</text>
  </g>
</svg>
</figure>

```
Input: grid = [[0,0,1,0,0,0,0,1,0,0,0,0,0],[0,0,0,0,0,0,0,1,1,1,0,0,0],[0,1,1,0,1,0,0,0,0,0,0,0,0],[0,1,0,0,1,1,0,0,1,0,1,0,0],[0,1,0,0,1,1,0,0,1,1,1,0,0],[0,0,0,0,0,0,0,0,0,0,1,0,0],[0,0,0,0,0,0,0,1,1,1,0,0,0],[0,0,0,0,0,0,0,1,1,0,0,0,0]]
Output: 6
Explanation: The answer is not 11, because the island must be connected 4-directionally.
```

**Example 2**

```
Input: grid = [[0,0,0,0,0,0,0,0]]
Output: 0
```

**Constraints**
```
m == grid.length
n == grid[i].length
1 <= m, n <= 50
grid[i][j] is either 0 or 1.
```

<!-- more -->

**Solution**

Apply BFS to compute the area of each island and compare with the max area value found so far. Here
is the working code in C#

```csharp
public class Solution
{
    public int MaxAreaOfIsland(int[][] grid)
    {
        // visited points
        var visited = new bool[grid.Length][];
        for (var r = 0; r < grid.Length; r++)
        {
            for (var c = 0; c < grid[r].Length; c++)
            {
                visited[r] = new bool[grid[r].Length];
            }
        }

        // use BFS to compute max area
        var max = 0;
        for (var r = 0; r < grid.Length; r++)
        {
            for (var c = 0; c < grid[r].Length; c++)
            {
                var current = grid[r][c];
                if (current != 1) continue;

                var area = ComputeArea(grid, (r, c), visited);
                if (area > max)
                    max = area;
            }
        }

        return max;
    }

    // Compute the area of the island, starting from the startPoint, using BFS
    public int ComputeArea(int[][] grid, (int, int) startPoint, bool[][] visited)
    {
        var area = 0;

        var q = new Queue<(int, int)>();
        q.Enqueue(startPoint);

        while (q.Any())
        {
            var (r, c) = q.Dequeue();
            if (visited[r][c])
                continue;

            visited[r][c] = true;
            area++;

            // go up
            if (r != 0 && grid[r - 1][c] == 1)
                q.Enqueue((r - 1, c));
            // go down
            if (r != grid.Length - 1 && grid[r + 1][c] == 1)
                q.Enqueue((r + 1, c));
            // go left
            if (c != 0 && grid[r][c - 1] == 1)
                q.Enqueue((r, c - 1));
            // go right
            if (c != grid[r].Length - 1 && grid[r][c + 1] == 1)
                q.Enqueue((r, c + 1));
        }

        return area;
    }

    // Main function to execute
    static async Task Main(string[] args)
    {
        var sol = new Solution();
        int[][] grid =
        {
            new[] { 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 },
            new[] { 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 },
            new[] { 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 },
            new[] { 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0 },
            new[] { 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0 },
            new[] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 },
            new[] { 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 },
            new[] { 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0 }
        };
        var res = sol.MaxAreaOfIsland(grid);
    }
}
```
