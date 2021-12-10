---
layout: post
title: "Solution to Rotting Oranges problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Rotting Oranges](https://leetcode.com/problems/rotting-oranges/)

You are given an `m x n` `grid` where each cell can have one of three values:

- `0` representing an empty cell,
- `1` representing a fresh orange, or
- `2` representing a rotten orange.

Every minute, any fresh orange that is **4-directionally adjacent** to a rotten orange becomes
rotten.

Return *the minimum number of minutes that must elapse until no cell has a fresh orange*. If *this is impossible*, return `-1`.

**Example 1**:

![Oranges](/files/2021-12-10-solution-to-rotting-oranges-problem/oranges.png)

```
Input: grid = [[2,1,1],[1,1,0],[0,1,1]]
Output: 4
```

**Example 2**:

```
Input: grid = [[2,1,1],[0,1,1],[1,0,1]]
Output: -1
Explanation: The orange in the bottom left corner (row 2, column 0) is never rotten, because rotting only happens 4-directionally.
```

**Example 3**:

```
Input: grid = [[0,2]]
Output: 0
Explanation: Since there are already no fresh oranges at minute 0, the answer is just 0.
```

**Constraints**:
```
m == grid.length
n == grid[i].length
1 <= m, n <= 10
grid[i][j] is 0, 1, or 2.
```

<!-- more -->

**Solution**: Use BFS to traverse all adjacent cells to the rotten cells. Instead of dequeuing one
item at a time, dequeue all items in the queue for each minute passed

Working code in C#

```csharp
public class Solution
{
    public int OrangesRotting(int[][] grid)
    {
        // queue used in BFS
        var q = new Queue<(int, int)>();
        var minute = 0;

        // enqueue the rotten cells first
        for (var i = 0; i < grid.Length; i++)
        {
            for (var j = 0; j < grid[i].Length; j++)
            {
                if (grid[i][j] == 2)
                    q.Enqueue((i, j));
            }
        }

        // bfs traversal, each loop is for one minute
        while (q.Any())
        {
            var tempQ = new Queue<(int, int)>();

            // dequeue all items in the current for this minute
            while (q.Any())
            {
                var (i, j) = q.Dequeue();

                // go up
                if (i != 0 && grid[i - 1][j] == 1)
                {
                    tempQ.Enqueue((i - 1, j));
                    grid[i-1][j] = 2;
                }
                // go down
                if (i != grid.Length - 1 && grid[i + 1][j] == 1)
                {
                    tempQ.Enqueue((i + 1, j));
                    grid[i + 1][j] = 2;
                }
                // go left
                if (j != 0 && grid[i][j - 1] == 1)
                {
                    tempQ.Enqueue((i, j - 1));
                    grid[i][j - 1] = 2;
                }
                // go right
                if (j != grid[i].Length - 1 && grid[i][j + 1] == 1)
                {
                    tempQ.Enqueue((i, j + 1));
                    grid[i][j + 1] = 2;
                }
            }

            if (!tempQ.Any()) continue;

            q = tempQ;
            minute++;
        }

        var anyFresh = false;
        foreach (var t in grid)
        {
            if (t.Any(t1 => t1 == 1))
                anyFresh = true;
        }

        return anyFresh ? -1 : minute;
    }
}
```
