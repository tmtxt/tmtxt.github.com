---
layout: post
title: "Solution to Longest Substring Without Repeating Characters problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

Given a string `s`, find the length of the **longest substring** without repeating characters.

Example 1
```
Input: s = "abcabcbb"
Output: 3
Explanation: The answer is "abc", with the length of 3.
```

Example 2
```
Input: s = "bbbbb"
Output: 1
Explanation: The answer is "b", with the length of 1.
```

Example 3
```
Input: s = "pwwkew"
Output: 3
Explanation: The answer is "wke", with the length of 3.
Notice that the answer must be a substring, "pwke" is a subsequence and not a substring.
```

Example 4
```
Input: s = ""
Output: 0
```

Constraints
```
0 <= s.length <= 5 * 104
s consists of English letters, digits, symbols and spaces.
```

Solution: Use Sliding Window strategy and a Hash. The Hash stores the characters and their index in
the string.

Maintain 2 pointers, the `low` and `high` pointer. Every time the `high` pointer is increased, store
the new character and its index to the Hash if not exists. Otherwise, move the `low` pointer to the last
duplicated character index.

This can be illustrated like this

<!-- more -->

![Alt Text](/files/2021-11-28-solution-to-longest-substring-without-repeating-characters-problem/1.png)

![Alt Text](/files/2021-11-28-solution-to-longest-substring-without-repeating-characters-problem/2.png)

![Alt Text](/files/2021-11-28-solution-to-longest-substring-without-repeating-characters-problem/3.png)

![Alt Text](/files/2021-11-28-solution-to-longest-substring-without-repeating-characters-problem/4.png)

![Alt Text](/files/2021-11-28-solution-to-longest-substring-without-repeating-characters-problem/5.png)

![Alt Text](/files/2021-11-28-solution-to-longest-substring-without-repeating-characters-problem/6.png)

![Alt Text](/files/2021-11-28-solution-to-longest-substring-without-repeating-characters-problem/7.png)

![Alt Text](/files/2021-11-28-solution-to-longest-substring-without-repeating-characters-problem/8.png)

```csharp
public class Solution {
    public int LengthOfLongestSubstring(string s)
    {
        var low = 0;
        var high = 0;

        // key is the character, value is the index
        var window = new Dictionary<char, int>();
        var max = 0;

        // move the high pointer
        for (high = 0; high < s.Length; high++)
        {
            var currentChar = s[high];

            if (!window.ContainsKey(currentChar))
            {
                // if the item is not presented in the window, add to the window
                window.Add(currentChar, high);

                // increase the longest string size if necessary
                if (window.Count > max)
                    max = window.Count;
            }
            else
            {
                // if the item is presented in the window
                // remove all items from low to last duplicated index
                var dupIndex = window[currentChar];
                for (; low < dupIndex; low++)
                    window.Remove(s[low]);

                // update the duplicated character index
                window[currentChar] = high;
                low++;
            }
        }

        return max;
    }
}
```
