---
layout: post
title: "Optimize A Recursive problem"
description: ""
categories: [algorithm]
tags: []
thumbnail:
---

Ok, you will probably find this problem during your coding interview 😩 Many companies don't like
giving the candidates the tricky problem. Instead, they will simply ask you to write a recursion
function to prove that you're a real engineer, not some random guys applying for the job because
it's well paid 😆 Often after writing that recursion function, the next question they will ask is
how to optimize that with a very large dataset and avoid the error Maximum call stack size
exceeded.

Here is how you can prepare yourself for that type of interview question

# 1. Basic Recursion

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

## An iterative solution

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

Simple? Yeah that's what they taught in university, but that's what will probably help you during
the interview.

## Tail-call optimization and Trampoline

Another way you can answer the interviewer is to use Tail-call optimization. Some programming
languages, if you return the the recursive expression, they can optimize this automatically by
avoiding creating a new call stack. From that, we can rewrite the above `sum` function in that style

```javascript
const sum = (arr, acc) => {
  if (arr.length === 0) {
    return acc;
  }
  const [head, ...tail] = arr;
  return sum(tail, head + acc);
};

sum(arr, 0);
```

However, a new problem arises here. The above piece of code is written in Javascript, which doesn't
support tail-call optimization

> Actually, it's partially supported now. However, let's assume we are working on a non-supported
> language.

There is another way you can rewrite the above one in a tail-call way without the support from the
language. Instead of returning the recursive call, you can return a function calling the recursive
function and use a another wrapper function (trampoline) to execute and manage the stack.

```javascript
const sum = (arr, acc) => {
  // base case, still the same
  if (arr.length === 0) {
    return acc;
  }

  const [head, ...tail] = arr;
  return () => sum(tail, head + acc);
};

const sumTrampoline = (arr) => {
  let result = sum(arr, 0);
  while (typeof result === 'function') {
    result = result();
  }
  return result;
};

sumTrampoline(arr);
```

# 2. Dynamic Programming

Now, let's come to a little bit more complicated problem

> You are climbing a staircase. It takes n steps to reach the top.
> Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?


- with memoization
- with tabulation

- to bfs