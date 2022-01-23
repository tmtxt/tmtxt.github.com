---
layout: post
title: "Solution to Reverse Bits problem"
description: ""
categories: [algorithm]
tags: []
thumbnail: 
---

> Leetcode: [Reverse Bits](https://leetcode.com/problems/reverse-bits/)

Reverse bits of a given 32 bits unsigned integer.

**Note**:
- Note that in some languages, such as Java, there is no unsigned integer type. In this case, both
input and output will be given as a signed integer type. They should not affect your
implementation, as the integer's internal binary representation is the same, whether it is
signed or unsigned.
- In Java, the compiler represents the signed integers using
[2's complement notation](https://en.wikipedia.org/wiki/Two%27s_complement). Therefore, in
**Example 2** above, the input represents the signed integer `-3` and the output represents the signed
integer `-1073741825`.

**Example 1**:
- Input: `n = 00000010100101000001111010011100`
- Output: `964176192` (`00111001011110000010100101000000`)
- Explanation: The input binary string `00000010100101000001111010011100` represents the unsigned
  integer `43261596`, so return `964176192` which its binary representation is
  `00111001011110000010100101000000`.

**Example 2**:
- Input: `n = 11111111111111111111111111111101`
- Output: `3221225471` (`10111111111111111111111111111111`)
- Explanation: The input binary string `11111111111111111111111111111101` represents the unsigned
  integer `4294967293`, so return `3221225471` which its binary representation is
  `10111111111111111111111111111111`.

**Constraints**:
- The input must be a **binary string** of length `32`

<!-- more -->

**Solution**

Let's do it the bitwise way

Working code in C#

```csharp
public class Solution {
    public uint reverseBits(uint n)
    {
        uint res = 0;
        for (int i = 0; i < 32; i++)
        {
            res <<= 1; // shift 1 bit of "res" left
            var lastBit = n & 1; // get the last bit of "n"
            res |= lastBit; // set to the last bit of "res"
            n >>= 1; // shift 1 bit of "n" right
        }

        return res;
    }
}
```
