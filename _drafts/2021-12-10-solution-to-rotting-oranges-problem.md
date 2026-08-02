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

Bright oranges are fresh (`1`), the brown ones are rotten (`2`), and grey cells are empty (`0`).
Each minute the rot spreads to 4-directionally adjacent fresh oranges. It takes `4` minutes for
every orange to rot:

<figure style="margin:0">
<svg width="872" height="180" viewBox="0 0 872 180" xmlns="http://www.w3.org/2000/svg" style="max-width:100%;height:auto">
  <defs>
    <g id="fresh">
      <circle cx="0" cy="0" r="16" fill="#fb8c00" stroke="#ef6c00" stroke-width="1"/>
      <path d="M0,-16 L0,-21" stroke="#5d4037" stroke-width="2"/>
      <ellipse cx="5" cy="-20" rx="5" ry="3" fill="#66bb6a" transform="rotate(-25 5 -20)"/>
      <circle cx="-5" cy="-5" r="2.5" fill="#ffcc80"/>
    </g>
    <g id="rotten">
      <circle cx="0" cy="0" r="16" fill="#6d4c41" stroke="#4e342e" stroke-width="1"/>
      <path d="M0,-16 L0,-21" stroke="#3e2723" stroke-width="2"/>
      <ellipse cx="5" cy="-20" rx="5" ry="3" fill="#8d6e63" transform="rotate(-25 5 -20)"/>
      <circle cx="-4" cy="3" r="2.5" fill="#3e2723"/>
      <circle cx="6" cy="-2" r="2" fill="#3e2723"/>
    </g>
    <marker id="gArrow" markerWidth="8" markerHeight="8" refX="6" refY="3" orient="auto">
      <path d="M0,0 L6,3 L0,6 Z" fill="#888"/>
    </marker>
  </defs>
  <!-- minute labels -->
  <g fill="#555" font-family="sans-serif" font-size="13" text-anchor="middle">
    <text x="76" y="18">Minute 0</text><text x="256" y="18">Minute 1</text><text x="436" y="18">Minute 2</text><text x="616" y="18">Minute 3</text><text x="796" y="18">Minute 4</text>
  </g>
  <!-- grid cells -->
  <g stroke="#999" stroke-width="1">
    <rect x="10"  y="30" width="44" height="44" fill="#ffffff"/><rect x="54"  y="30" width="44" height="44" fill="#ffffff"/><rect x="98"  y="30" width="44" height="44" fill="#ffffff"/>
    <rect x="10"  y="74" width="44" height="44" fill="#ffffff"/><rect x="54"  y="74" width="44" height="44" fill="#ffffff"/><rect x="98"  y="74" width="44" height="44" fill="#eeeeee"/>
    <rect x="10"  y="118" width="44" height="44" fill="#eeeeee"/><rect x="54"  y="118" width="44" height="44" fill="#ffffff"/><rect x="98"  y="118" width="44" height="44" fill="#ffffff"/>
    <rect x="190" y="30" width="44" height="44" fill="#ffffff"/><rect x="234" y="30" width="44" height="44" fill="#ffffff"/><rect x="278" y="30" width="44" height="44" fill="#ffffff"/>
    <rect x="190" y="74" width="44" height="44" fill="#ffffff"/><rect x="234" y="74" width="44" height="44" fill="#ffffff"/><rect x="278" y="74" width="44" height="44" fill="#eeeeee"/>
    <rect x="190" y="118" width="44" height="44" fill="#eeeeee"/><rect x="234" y="118" width="44" height="44" fill="#ffffff"/><rect x="278" y="118" width="44" height="44" fill="#ffffff"/>
    <rect x="370" y="30" width="44" height="44" fill="#ffffff"/><rect x="414" y="30" width="44" height="44" fill="#ffffff"/><rect x="458" y="30" width="44" height="44" fill="#ffffff"/>
    <rect x="370" y="74" width="44" height="44" fill="#ffffff"/><rect x="414" y="74" width="44" height="44" fill="#ffffff"/><rect x="458" y="74" width="44" height="44" fill="#eeeeee"/>
    <rect x="370" y="118" width="44" height="44" fill="#eeeeee"/><rect x="414" y="118" width="44" height="44" fill="#ffffff"/><rect x="458" y="118" width="44" height="44" fill="#ffffff"/>
    <rect x="550" y="30" width="44" height="44" fill="#ffffff"/><rect x="594" y="30" width="44" height="44" fill="#ffffff"/><rect x="638" y="30" width="44" height="44" fill="#ffffff"/>
    <rect x="550" y="74" width="44" height="44" fill="#ffffff"/><rect x="594" y="74" width="44" height="44" fill="#ffffff"/><rect x="638" y="74" width="44" height="44" fill="#eeeeee"/>
    <rect x="550" y="118" width="44" height="44" fill="#eeeeee"/><rect x="594" y="118" width="44" height="44" fill="#ffffff"/><rect x="638" y="118" width="44" height="44" fill="#ffffff"/>
    <rect x="730" y="30" width="44" height="44" fill="#ffffff"/><rect x="774" y="30" width="44" height="44" fill="#ffffff"/><rect x="818" y="30" width="44" height="44" fill="#ffffff"/>
    <rect x="730" y="74" width="44" height="44" fill="#ffffff"/><rect x="774" y="74" width="44" height="44" fill="#ffffff"/><rect x="818" y="74" width="44" height="44" fill="#eeeeee"/>
    <rect x="730" y="118" width="44" height="44" fill="#eeeeee"/><rect x="774" y="118" width="44" height="44" fill="#ffffff"/><rect x="818" y="118" width="44" height="44" fill="#ffffff"/>
  </g>
  <!-- arrows between minutes -->
  <g stroke="#888" stroke-width="2" marker-end="url(#gArrow)">
    <line x1="146" y1="96" x2="184" y2="96"/><line x1="326" y1="96" x2="364" y2="96"/><line x1="506" y1="96" x2="544" y2="96"/><line x1="686" y1="96" x2="724" y2="96"/>
  </g>
  <!-- minute 0 -->
  <use href="#rotten" x="32"  y="52"/><use href="#fresh" x="76"  y="52"/><use href="#fresh" x="120" y="52"/>
  <use href="#fresh"  x="32"  y="96"/><use href="#fresh" x="76"  y="96"/>
  <use href="#fresh"  x="76"  y="140"/><use href="#fresh" x="120" y="140"/>
  <!-- minute 1 -->
  <use href="#rotten" x="212" y="52"/><use href="#rotten" x="256" y="52"/><use href="#fresh" x="300" y="52"/>
  <use href="#rotten" x="212" y="96"/><use href="#fresh"  x="256" y="96"/>
  <use href="#fresh"  x="256" y="140"/><use href="#fresh"  x="300" y="140"/>
  <!-- minute 2 -->
  <use href="#rotten" x="392" y="52"/><use href="#rotten" x="436" y="52"/><use href="#rotten" x="480" y="52"/>
  <use href="#rotten" x="392" y="96"/><use href="#rotten" x="436" y="96"/>
  <use href="#fresh"  x="436" y="140"/><use href="#fresh"  x="480" y="140"/>
  <!-- minute 3 -->
  <use href="#rotten" x="572" y="52"/><use href="#rotten" x="616" y="52"/><use href="#rotten" x="660" y="52"/>
  <use href="#rotten" x="572" y="96"/><use href="#rotten" x="616" y="96"/>
  <use href="#rotten" x="616" y="140"/><use href="#fresh"  x="660" y="140"/>
  <!-- minute 4 -->
  <use href="#rotten" x="752" y="52"/><use href="#rotten" x="796" y="52"/><use href="#rotten" x="840" y="52"/>
  <use href="#rotten" x="752" y="96"/><use href="#rotten" x="796" y="96"/>
  <use href="#rotten" x="796" y="140"/><use href="#rotten" x="840" y="140"/>
</svg>
</figure>

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
