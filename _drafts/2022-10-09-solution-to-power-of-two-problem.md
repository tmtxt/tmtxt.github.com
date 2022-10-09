---
layout: post
title: "Solution to Power of Two problem"
description: "Who the hell could think of this bitwise solution?"
categories: [algorithm]
thumbnail:
---

> Leetcode: [Power of Two](https://leetcode.com/problems/power-of-two/)

Given an integer `n`, return `true` if it is a *power of two*. Otherwise, return `false`.

An integer `n` is a power of two, if there exists an integer `x` such that `n == 2^x`.

**Example 1**:
```
Input: n = 1
Output: true
Explanation: 20 = 1
```

**Example 2**:
```
Input: n = 16
Output: true
Explanation: 24 = 16
```

**Example 3**:
```
Input: n = 3
Output: false
```

**Constraints**:
```
-231 <= n <= 231 - 1
```

**Follow up**: Could you solve it without loops/recursion?

<!-- more -->

**Solution**

Loops/Recursion seems quite straight forward. Let's talk about bitwise operators.

> This is the first time I have ever used this...

Let's take a look at the some numbers that are the power of two and their binary representation

| Decimal | Binary |
|---------|--------|
| 2       | 10     |
| 4       | 100    |
| 8       | 1000   |
| 16      | 10000  |
| 32      | 100000 |
{: .table }

For all numbers, the first bit are `true` and all the remaining bits are `false`. You can check the
binary representation based on that condition, but it's still a loop on the binary string.

If we minus each number by 1, we get a new number with all bits are `true` (all bits are inverted).

| Decimal | Binary |
|---------|--------|
| 2-1     | 1      |
| 4-1     | 11     |
| 8-1     | 111    |
| 16-1    | 1111   |
| 32-1    | 11111  |
{: .table }

If we use `&` bitwise operator between `n` and `n-1`, the result will be `0`.

| Decimal | Binary          | Result |
|---------|-----------------|--------|
| 2 & 1   | 10 & 01         | 00     |
| 4 & 3   | 100 & 011       | 000    |
| 8 & 7   | 1000 & 0111     | 0000   |
| 16 & 15 | 10000 & 01111   | 00000  |
| 32 & 31 | 100000 & 011111 | 000000 |
{: .table }

Working code in C#

```csharp
public class Solution {
    public bool IsPowerOfTwo(int n)
    {
        return n > 0 && (n & (n - 1)) == 0;
    }
}
```

> Who the hell could think of this bitwise solution?
