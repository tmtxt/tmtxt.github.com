---
layout: post
title: "Solution to 01 Matrix problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [01 Matrix](https://leetcode.com/problems/01-matrix/)

Given an `m x n` binary matrix `mat`, return *the distance of the nearest `0` for each cell*.

The distance between two adjacent cells is `1`.

**Example 1**:

![01](/files/2021-12-09-solution-to-01-matrix-problem/01-1-grid.jpg)

```
Input: mat = [[0,0,0],[0,1,0],[0,0,0]]
Output: [[0,0,0],[0,1,0],[0,0,0]]
```

**Example 2**:

![02](/files/2021-12-09-solution-to-01-matrix-problem/01-2-grid.jpg)

```
Input: mat = [[0,0,0],[0,1,0],[1,1,1]]
Output: [[0,0,0],[0,1,0],[1,2,1]]
```

**Constraints**:
- `m == mat.length`
- `n == mat[i].length`
- `1 <= m, n <= 104`
- `1 <= m * n <= 104`
- `mat[i][j]` is either `0` or `1`.
- There is at least one `0` in `mat`.

<!-- more -->

**Solution**: Use BFS to find the nearest 0 from 1.

Working code in C#, not the perfect one, but accepted on Leetcode

```csharp
public class Solution
{
    public int[][] UpdateMatrix(int[][] mat)
    {
        // init the res
        var res = new int[mat.Length][];
        for (var i = 0; i < res.Length; i++)
        {
            res[i] = new int[mat[i].Length];
        }

        // do bfs for each cell
        for (var i = 0; i < res.Length; i++)
        {
            for (var j = 0; j < res[i].Length; j++)
            {
                // no need to do bfs for cell that is already 0
                if (mat[i][j] == 0)
                {
                    res[i][j] = 0;
                }
                else
                {
                    var bfsVal = Bfs(i, j, mat);
                    res[i][j] = bfsVal;
                }
            }
        }

        return res;
    }

    private int Bfs(int i, int j, int[][] mat)
    {
        var q = new Queue<(int, int, int)>();
        q.Enqueue((i, j, 0));

        // use 2d array here instead of array of array
        // this is just a workaround. changing this to array of array will result
        // in a Time Limit Exceeded error on Leetcode
        var visited = new bool[mat.Length, mat[0].Length];

        while (q.Any())
        {
            var (i0, j0, count) = q.Dequeue();
            visited[i0,j0] = true;
            if (mat[i0][j0] == 0) return count;

            // go up
            if (i0 != 0 && !visited[i0 - 1,j0])
                q.Enqueue((i0 - 1, j0, count + 1));
            // go down
            if (i0 != mat.Length - 1 && !visited[i0 + 1,j0])
                q.Enqueue((i0 + 1, j0, count + 1));
            // go left
            if (j0 != 0 && !visited[i0,j0 - 1])
                q.Enqueue((i0, j0 - 1, count + 1));
            // go right
            if (j0 != mat[i0].Length - 1 && !visited[i0,j0 + 1])
                q.Enqueue((i0, j0 + 1, count + 1));
        }

        return 0;
    }
}
```

So annoying that Leetcode always use array of array instead of 2d array so I have to write the for
loop to initialize the array of array instead of just one command to allocate the 2d array with
default values!
