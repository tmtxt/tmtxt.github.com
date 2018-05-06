---
layout: post
title: "Solutions to Egg Drop problem"
description: ""
categories: [algorithm]
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course.

# Question

Suppose that you have an nnn-story building (with floors 1 through nnn) and plenty of eggs. An egg
breaks if it is dropped from floor TTT or higher and does not break otherwise. Your goal is to
devise a strategy to determine the value of TTT given the following limitations on the number of
eggs and tosses

# Solution 1

> 1 egg, ≤T tosses

This is an `O(n)` solution. Just do a simple loop from the first floor until the egg break.

# Solution 2

> ∼1lgN eggs, ∼1lgN tosses

Use Binary search strategy.

- Start from the middle floor, drop the egg
- If it breaks, repeat with the lower half
- Otherwise, repeat with the higher half

# Solution 3

> ∼lgT eggs, ∼2lgT tosses

- Drop the at floor 1, 2, 4, 8, 16,... until it breaks
- If the egg drop at level 32, that mean T must be between 16 and 32 (between the floor of last toss
  and the floor of this toss). At this time, you have used
  - `1` egg
  - `lgT` tosses because you double the floor number each time
- Perform a binary search from floor 16 to 32.
  - The binary search will cost another `lgT` tosses
  - Because you do binary search on half of the floor (16 to 32 in this case, not all 32 floors),
    you need to use `lgT - 1` eggs.
- In total, it will take you `~lgT` eggs and `~2lgT` tosses.
