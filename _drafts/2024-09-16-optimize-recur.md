---
layout: post
title: "Optimize A Recursive function"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

Ok, you will probably find this problem during your coding interview 😩 Many companies don't like
giving the candidates the tricky problem. Instead, they will simply ask you to write a recursion
function to prove that you're a real engineer, not some random guys applying for the job because
it's well paid 😆 Often after writing that recursion function, the next question they will ask is
how do you optimize that with a very large dataset and avoid the error Maximum call stack size
exceeded.

Here is how you can prepare yourself for that type of interview question

Let's start with this very basic recursion question

> Given an array arr with N items, write a recursive function to calculate the sum of all items in
> the array

If you think about recursion in its simplest form, it's like a divide and conquer strategy,
where you break down the problem into smaller ones, solve one of them first and then repeat with
the other remaining ones (recursive case) until there is no item left (base case). Now, let's take
a look at the above problem, the solution can be described like this: The sum of an array is the sum
of the first element (the head of the list) and all the remaining items (the tail of the array).

> If you have worked in any lisp-like language before (Emacs Lisp for example 😂), you will
> immediately see the pattern. They are the `car` and `cdr` function in Elisp.

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

# An iterative solution

Of course, the easiest way is to convert the above one to a simple `for` (or `while`) loop to
avoid adding to the call stack.

```javascript
const sum = (arr) => {
  let sum = 0;
  for (let i=0; i<arr.length; i++) {
    sum += arr[i];
  }
  return sum;
};
```

Now, let's come to a little bit more complicated problem

> You are climbing a staircase. It takes n steps to reach the top.
> Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?

- convert to a loop
- with memoization
- tail-call optimization
- trampoline
- to bfs