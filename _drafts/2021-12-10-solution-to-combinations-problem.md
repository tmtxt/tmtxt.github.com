---
layout: post
title: "Solution to Combinations problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [Combinations](https://leetcode.com/problems/combinations/)

Given two integers `n` and `k`, return
*all possible combinations of `k` numbers out of the range `[1, n]`*.

You may return the answer in any order.

**Example 1**:

```
Input: n = 4, k = 2
Output:
[
  [2,4],
  [3,4],
  [2,3],
  [1,2],
  [1,3],
  [1,4],
]
```

**Example 2**:

```
Input: n = 1, k = 1
Output: [[1]]
```

**Constraints**:
```
1 <= n <= 20
1 <= k <= n
```

<!-- more -->

**Solutions**: Use Backtracking.

Illustration image from [here](https://www.tutorialcup.com/leetcode-solutions/combinations-leetcode-solution.htm)

![Backtracking](/files/2021-12-10-solution-to-combinations-problem/backtrack.png)

Working code in C#

```csharp
public class Solution
{
    public IList<IList<int>> Combine(int n, int k)
    {
        var res = new List<IList<int>>();
        Backtrack(1, n, k, new List<int>(), res);
        return res;
    }

    public void Backtrack(int i, int n, int k, IList<int> comb, IList<IList<int>> res)
    {
        if (comb.Count == k)
        {
            res.Add(comb.ToList());
        }
        else
        {
            for (var j = i; j <= n; j++)
            {
                comb.Add(j);
                Backtrack(j + 1, n, k, comb, res);
                comb.Remove(j);
            }
        }
    }
}
```
