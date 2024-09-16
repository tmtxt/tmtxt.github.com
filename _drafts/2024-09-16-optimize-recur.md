---
layout: post
title: "Optimize A Recursive function"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

Recursion can be used to solve problem in an interesting and elegant way. For me, it's like a
divide and conquer strategy, where you break down the problem into smaller ones, solve one of them
first and then repeat with the other remaining ones.

> Ok, you will probably find this problem during your coding interview 😩

Let's start with this very simple recursion question

> Given an array arr with N items, write a recursive function to calculate the sum of all items in
> the array

Now, let's look at the problem from the recursive perspective. If you have worked in any lisp-like
language before (Emacs Lisp for example 😂), you will immediately see the pattern. The solution
could be described like this: The sum of the array is the sum of the first element (the head of the
list) and all the remaining items (the tail of the array).

From that, you can easily write the basic recursive function like this (in Javascript)

```javascript
const sum = (arr) => {
  if (arr.length === 0) {
    return 0;
  }

  const [head, ...tail] = arr;
  return head + sum(tail);
};
```