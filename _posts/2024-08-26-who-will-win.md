---
layout: post
title: "Who will win?"
description: "Another interview question likely?"
categories: [javascript]
tags: []
---

My friend sent me this and asked me what the value printed at the end is, which promise would win
the race.

```javascript
const promise1 = new Promise((resolve) => {
  for (let i = 0; i < 10000; i++) {} // longer loop
  resolve('promise1');
});

const promise2 = new Promise((resolve) => {
  for (let i = 0; i < 100; i++) {} // shorter loop
  resolve('promise2');
});

Promise.race([promise1, promise2]).then((value) => console.log(value));
```

Surprisingly, most of their answers are the same, with `promise2` to be printed at the end!

If you have been working with Nodejs long enough and understand its event loop, you can immediately
see the problem in the above code. Even though the code is inside a Promise, which should be run
**concurrently** (in normal mindset). However, the `for` loops contain nothing inside. They are
called blocking code in js. Since Nodejs is single-threaded, the above functions will be executed
sequentially, in the order of declaration.

It will behave differently only when you add some non-blocking (async) operation inside the
Promise, for example `setInterval`.

Turns out a lot of them also use ChatGPT to get the answer

![ChatGPT](/files/2024-08-26-who-will-win/chatgpt.png)

> Use these AIs to speed up and improve your workflow, but don't completely trust them. Always
> verify the answers yourself.