---
layout: post
title: "Solution to First Bad Version problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Leetcode: [First Bad Version](https://leetcode.com/problems/first-bad-version/)

You are a product manager and currently leading a team to develop a new product. Unfortunately, the
latest version of your product fails the quality check. Since each version is developed based on the
previous version, all the versions after a bad version are also bad.

Suppose you have n versions `[1, 2, ..., n]` and you want to find out the first bad one, which
causes all the following ones to be bad.

You are given an API `bool isBadVersion(version)` which returns whether version is bad. Implement a
function to find the first bad version. You should minimize the number of calls to the API.

**Example 1**:
```
Input: n = 5, bad = 4
Output: 4
Explanation:
call isBadVersion(3) -> false
call isBadVersion(5) -> true
call isBadVersion(4) -> true
Then 4 is the first bad version.
```

**Example 2**:
```
Input: n = 1, bad = 1
Output: 1
```

**Constraints**:
```
1 <= bad <= n <= 231 - 1
```

<!-- more -->

**Solution**

Simply use Binary Search. Too simple so I won't explain much.

Working code in C#:
```csharp
/* The isBadVersion API is defined in the parent class VersionControl.
      bool IsBadVersion(int version); */
public class Solution : VersionControl
{
    public int FirstBadVersion(int n)
    {
        int left = 1;
        int right = n;

        while (left < right)
        {
            int mid = left + (right - left) / 2;
            if (IsBadVersion(mid))
            {
                right = mid;
            }
            else
            {
                left = mid + 1;
            }
        }

        return left;
    }
}
```

This is just another form of the Egg Drop problem, described here [Solutions to Egg Drop problem]({% post_url 2018-05-06-solutions-to-egg-drop-problem %}).
