---
layout: post
title: "Solutions to Egg Drop problem"
description: ""
categories: [algorithm]
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course.

# Question

Suppose that you have an nnn-story building (with floors 1 through nnn) and plenty of eggs. An egg
breaks if it is dropped from floor T or higher and does not break otherwise. Your goal is to
devise a strategy to determine the value of T given the following limitations on the number of
eggs and tosses

# Solution 1

> 1 egg, ≤T tosses

This is an `O(n)` solution. Just do a simple loop from the first floor until the egg break.

# Solution 2

> ∼1lgN eggs, ∼1lgN tosses

Use Binary search strategy.

- Start from the middle floor, drop the egg
- If it breaks, repeat with the lower half
- Otherwise, repeat with the upper half

# Solution 3

> ∼lgT eggs, ∼2lgT tosses

<!-- more -->

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

# Solution 4

> 2 eggs, 2<math xmlns="http://www.w3.org/1998/Math/MathML"><msqrt><mi>n</mi></msqrt></math> tosses

To make it easy to imagine, let's take **n = 100**,
so **<math xmlns="http://www.w3.org/1998/Math/MathML"><msqrt><mi>n</mi></msqrt></math> = 10**

- Drop the egg at
  **<math xmlns="http://www.w3.org/1998/Math/MathML"><msqrt><mi>n</mi></msqrt></math>**
  floor (level 10 in this case)
- If it doesn't break, increase the floor by
  **<math xmlns="http://www.w3.org/1998/Math/MathML"><msqrt><mi>n</mi></msqrt></math>**
  and repeat until the egg breaks
  - Until that, you have used maximum
    **<math xmlns="http://www.w3.org/1998/Math/MathML"><msqrt><mi>n</mi></msqrt></math> = 10**
    tosses and **1** egg
- Now you know the range that can make egg break. That range size is
  **<math xmlns="http://www.w3.org/1998/Math/MathML"><msqrt><mi>n</mi></msqrt></math>**
  - For example, the egg breaks at the floor **60**, that mean **T** must be between **50** and
    **60**
- Do a sequential search in that range, use the other remaining egg. Because the length of that
  range is **<math xmlns="http://www.w3.org/1998/Math/MathML"><msqrt><mi>n</mi></msqrt></math>**,
  it takes you maximum another
  **<math xmlns="http://www.w3.org/1998/Math/MathML"><msqrt><mi>n</mi></msqrt></math> = 10** tosses
- The total running time is
  **2<math xmlns="http://www.w3.org/1998/Math/MathML"><msqrt><mi>n</mi></msqrt></math>**
  and takes **2** eggs

# Solution 5

> 2 eggs, <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>&le;c</mi><msqrt><mi>T</mi></msqrt></math> tosses

I still haven't thought of the solution for this. Here is the hint that Coursera gave me

<math xmlns="http://www.w3.org/1998/Math/MathML">
  <mi>
    1 + 2 + 3 + ... + t ~
  </mi>
  <mspace />
  <mfrac>
    <mi>1</mi>
    <mi>2</mi>
  </mfrac>
  <msup>
    <mi>t</mi>
    <mn>2</mn>
  </msup>
</math>
Aim for <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>c = 2</mi><msqrt><mi>2</mi></msqrt></math>
