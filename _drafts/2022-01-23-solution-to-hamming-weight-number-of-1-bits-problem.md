---
layout: post
title: "Solution to Hamming weight (Number of 1 Bits) problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Number of 1 Bits](https://leetcode.com/problems/number-of-1-bits/)

Write a function that takes an unsigned integer and returns the number of `1` bits it has (also
known as the
[Hamming weight](https://en.wikipedia.org/wiki/Hamming_weight)).

**Note**:
- Note that in some languages, such as Java, there is no unsigned integer type. In this case, the
  input will be given as a signed integer type. It should not affect your implementation, as the
  integer's internal binary representation is the same, whether it is signed or unsigned.
- In Java, the compiler represents the signed integers using
[2's complement notation](https://en.wikipedia.org/wiki/Two%27s_complement). Therefore, in
**Example 3**, the input represents the signed integer. `-3`.

**Example 1**:
```
Input: n = 00000000000000000000000000001011
Output: 3
Explanation: The input binary string 00000000000000000000000000001011 has a total of three '1' bits.
```

**Example 2**:
```
Input: n = 00000000000000000000000010000000
Output: 1
Explanation: The input binary string 00000000000000000000000010000000 has a total of one '1' bit.
```

**Example 3**:
```
Input: n = 11111111111111111111111111111101
Output: 31
Explanation: The input binary string 11111111111111111111111111111101 has a total of thirty one '1' bits.
```

**Constraints**:
- The input must be a **binary string** of length `32`.

<!-- more -->

**Solution**

Let's skip the loop solution and do it the *bitwise* way.

Let's say we want to check if the last bit (the right most one) is `1` or `0`, we need to use
`&` operator between that number and `1`. For example

```
11001 & 1 = 1
11000 & 1 = 0
```

If we keep shifting the bit one by one to the right and repeat the above step, we will get the
result `1` every time a `1` reach the right most position

```
11001 & 1 = 1
01100 & 1 = 0
00110 & 1 = 0
00011 & 1 = 1
00001 & 1 = 1
```

So what we need to do is keep shifting the bit to the right until there is no more bit to shift and
then count how many times `1` appears as the result

Working code in C#

```csharp
public class Solution
{
    public int HammingWeight(uint n)
    {
        long res = 0;
        while (n != 0)
        {
            res += n & 1;
            n >>= 1;
        }

        return (int)res;
    }
}
```
