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

![Grid](/files/2021-11-29-solution-to-max-area-of-island-problem/maxarea1-grid.jpg)

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
