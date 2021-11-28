---
layout: post
title: "Solution to Permutation in String problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

Given two strings `s1` and `s2`, return `true` *if* `s2` *contains a permutation of* `s1`, or `false` otherwise.

In other words, return `true` if one of `s1'`s permutations is the substring of `s2`.

Example 1
```
Input: s1 = "ab", s2 = "eidbaooo"
Output: true
Explanation: s2 contains one permutation of s1 ("ba").
```

Example 2
```
Input: s1 = "ab", s2 = "eidboaoo"
Output: false
```

Constraints
```
1 <= s1.length, s2.length <= 104
s1 and s2 consist of lowercase English letters.
```

Solution: Use Sliding Window strategy and Hash data structure.

Loop through `s2` with window size of `s1.length`. Use a Hash to store the occurrence of the
characters in the current window and compare with the input string `s1` to see if the current window
is the permutation of s1. For each loop, decrease the count of the last removing item and increase
the count of the new item.

<!-- more -->

```csharp
public class Solution {
    public bool CheckInclusion(string s1, string s2)
    {
        // Edge case
        if (s2.Length < s1.Length)
            return false;

        // Convert the input string s1 to a Hash for faster permutation check
        var permutationDict = new Dictionary<char, int>();
        foreach (var c in s1)
        {
            if (!permutationDict.ContainsKey(c))
                permutationDict[c] = 0;
            permutationDict[c]++;
        }

        // The Hash to store the occurrence of the items in the current window
        var windowDict = new Dictionary<char, int>();

        // The first window
        for (var i = 0; i < s1.Length; i++)
        {
            var currentChar = s2[i];
            Increase(windowDict, currentChar);

            if (IsPermutation(permutationDict, windowDict))
                return true;
        }

        // Start sliding window from s1.Length
        for (var i = s1.Length; i < s2.Length; i++)
        {
            var addingChar = s2[i];
            var removingChar = s2[i - s1.Length];

            Decrease(windowDict, removingChar);
            Increase(windowDict, addingChar);

            if (IsPermutation(permutationDict, windowDict))
                return true;
        }

        return false;
    }

    // Increase the count of the key in windowDict
    public void Increase(Dictionary<char, int> windowDict, char key)
    {
        if (!windowDict.ContainsKey(key))
            windowDict[key] = 0;

        windowDict[key]++;
    }

    // Decrease the count of the key in windowDict
    public void Decrease(Dictionary<char, int> windowDict, char key)
    {
        if (!windowDict.ContainsKey(key))
            return;

        windowDict[key]--;
        if (windowDict[key] == 0)
            windowDict.Remove(key);
    }

    // Check the occurrence of the items in permutationDict and windowDict to see windowDict is the
    // permutation of permutationDict
    public bool IsPermutation(Dictionary<char, int> permutationDict, Dictionary<char, int> windowDict)
    {
        if (permutationDict.Count != windowDict.Count)
            return false;

        foreach (var (key, value) in permutationDict)
        {
            if (!windowDict.ContainsKey(key))
                return false;

            if (windowDict[key] != value)
                return false;
        }

        return true;
    }
}
```
