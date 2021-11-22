---
layout: post
title: "Longest Valid Parentheses solution"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

> Another algorithm exercise from my friend

Given a string containing just the characters `(` and `)`, find the length of the longest valid
(well-formed) parentheses substring.

Example 1:

```
Input: s = "(()"
Output: 2
Explanation: The longest valid parentheses substring is "()".
```

Example 2:

```
Input: s = ")()())"
Output: 4
Explanation: The longest valid parentheses substring is "()()".
```

Example 3:

```
Input: s = ""
Output: 0
```

Example 4:

```
Input: s = ")(())())()"
Output: 6
Explanation: The longest valid parentheses substring is "(())()".
```

Constraints:
- `0 <= s.length <= 3 * 10^4`
- `s[i]` is `(` or `)`

<!-- more -->

Let's just skip Bruteforce solution!

Here is the solution that I came up with.
- Introduce these variables `max`, `openCount` and `closeCount`
- A valid (and nested) string should have `(` and `)` equal, in the correct order
- Loop from the beginning of the string
- If you find `(`, increase `openCount`. If you find `)`, increase `closeCount`
- If `openCount` = `closeCount`, you found a valid substring. Continue if the next item is `(` to
  see if subsequent items can be concatenated into this.
- If `closeCount` > `openCount`, stop because the sub string has become invalid
- If you find `(` while increasing `closeCount`, also stop.
- Time complexity: `O(n)`, just one loop
- No extra space usage
