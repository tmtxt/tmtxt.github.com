---
layout: post
title: "Implement Javascript Promise from scratch"
description: "Just an interview question... 🙃"
categories: [javascript]
tags: []
thumbnail:
---

Just an interview question 🙃

> The interview question: Re-implement Javascript Promise from scratch. The implementation should
> support **chaining** (of course, asynchronously).

# Promise API

> It has been too long since I started using `async`/`await`. I almost forgot these Promise APIs.

Let's revisit the basic Javascript
[Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
APIs

```javascript
const myPromise = new Promise((resolve, reject) => {
  setTimeout(() => {
    resolve('foo');
  }, 300);
});

// try catch each one separately
myPromise
  .then(handleResolvedA, handleRejectedA)
  .then(handleResolvedB, handleRejectedB)
  .then(handleResolvedC, handleRejectedC);

// or catch the first one if any
myPromise
  .then(handleResolvedA)
  .then(handleResolvedB)
  .then(handleResolvedC)
  .catch(handleRejectedAny);
```

# A very basic implementation

Here is a trivial implementation with just `resolve` and `reject` support
- A `MyPromise` class
- A constructor that provides `resolve` and `reject` functions for the executor

```typescript
type ResolveFn = (res: any) => void;
type RejectFn = (err: any) => void;
type PromiseExecuteFn = (resolve: ResolveFn, reject: RejectFn) => void;

export default class MyPromise {
  constructor(execute: PromiseExecuteFn) {
    const resolve: ResolveFn = (res) => {
      console.log(`Resolved ${res}`);
    };
    const reject: RejectFn = (err) => {
      console.log(`Rejected ${err}`);
    };

    try {
      execute(resolve, reject);
    } catch (err) {
      reject(err);
    }
  }
}

const resolvePromise = new MyPromise((resolve, reject) => {
  resolve('success');
});

const rejectPromise = new MyPromise((resolve, reject) => {
  reject('error');
});
```

# Adding then support

`then` support for Promise is not difficult but a bit tricky since you have to handle async tasks.
- Implement the `then` function with `handleResolved` and `handleRejected` params
- Defer the execution of those 2 functions until the promise has been resolved or rejected
- One thing to notice is that each promise can only be resolved or rejected once so you
will need to store its status somewhere.

Let's look at the updated version

```typescript
type ResolveFn = (res: any) => void;
type RejectFn = (err: any) => void;
type PromiseExecuteFn = (resolve: ResolveFn, reject: RejectFn) => void;
type HandleResolvedFn = (res: any) => any;
type HandleRejectedFn = (err: any) => any;

export default class MyPromise {
  private status: 'init' | 'resolved' | 'rejected' = 'init';
  private handleResolved: HandleResolvedFn;
  private handleRejected: HandleRejectedFn;

  constructor(execute: PromiseExecuteFn) {
    const resolve: ResolveFn = (res) => {
      if (this.status !== 'init') return; // only resolve/reject once

      this.status = 'resolved';
      this.handleResolved && this.handleResolved(res);
    };
    const reject: RejectFn = (err) => {
      if (this.status !== 'init') return; // only resolve/reject once
      this.status = 'rejected';
      this.handleRejected && this.handleRejected(err);
    };

    try {
      execute(resolve, reject);
    } catch (err) {
      reject(err);
    }
  }

  then(handleResolved: HandleResolvedFn, handleRejected?: HandleRejectedFn) {
    // don't call these handleResolved and handleRejected function here
    // defer them until the promise is resolved or rejected
    this.handleResolved = handleResolved;
    this.handleRejected = handleRejected;
  }
}


const resolvePromise = new MyPromise((resolve, reject) => {
  setTimeout(() => resolve('success'), 300);
}).then((value) => console.log(value));

const rejectPromise = new MyPromise((resolve, reject) => {
  setTimeout(() => reject('error'), 300);
}).then(
  (value) => {},
  (err) => console.log(err)
);
```

# Chaining support

Chaining support is probably the most complicated part. 😅
In order to make the `then` function chainable, you need to return
another **Promise**, which wraps the `handleResolved` and `handleRejected` functions.
- The `then` function will return another Promise (the inner Promise)
- The inner Promise wraps the `handleResolved` function and resolves itself when the outer Promise
  has been resolved
- The inner Promise wraps the `handleRejected` function and rejects itself when the outer Promise
  has been rejected

Here is the first implementation of the `then` function

```typescript
then(handleResolved: HandleResolvedFn, handleRejected?: HandleRejectedFn) {
  return new MyPromise((resolve, reject) => {
    // wrap the handleResolved function and resolve this one
    this.handleResolved = (outerRes) => {
      try {
        const innerRes = handleResolved(outerRes);
        resolve(innerRes);
      } catch (e) {
        reject(e);
      }
    };
    // wrap the handleRejected function and reject this one
    this.handleRejected = (outerErr) => {
      try {
        // you need this if/else so in case the then doesn't provide the
        // handleRejected function, the error will be cascaded downstream
        if (handleRejected) {
          const innerErr = handleRejected(outerErr);
          reject(innerErr);
        } else {
          reject(outerErr);
        }
      } catch (e) {
        reject(e);
      }
    };
  });
}
```

Here is a basic test case

```typescript
import MyPromise from './promise';

new MyPromise((resolve, reject) => {
  setTimeout(() => resolve('success 1'), 300);
})
  .then((value) => {
    console.log(value);
    return 'sucesss 2';
  })
  .then((value) => {
    console.log(value);
    return 'success 3';
  })
  .then((value) => {
    console.log(value);
  });
// This will print
// success 1
// success 2
// success 3
```

Other test cases, see below.

# Promise of Promise

The `handleResolved` and `handleRejected` functions can also return a Promise, sghhhhh. 🥲

Ok, simply add a check if `handleResolved` returns a Promise and let it resolve/reject itself.

```typescript
this.handleResolved = (outerRes) => {
  try {
    const innerRes = handleResolved(outerRes);
    if (innerRes instanceof MyPromise) {
      // Promise of Promise of Promise
      innerRes.then(resolve, reject);
    } else {
      resolve(innerRes);
    }
  } catch (e) {
    reject(e);
  }
};
```

and here is how to test

```typescript
new MyPromise((resolve, reject) => {
  setTimeout(() => resolve('success 1'), 300);
})
  .then((value) => {
    console.log(`resolve ${value}`);
    return new MyPromise((resolve, reject) => {
      setTimeout(() => resolve('success 2'), 300);
    });
  })
  .then((value) => {
    console.log(`resolve ${value}`);
  });
// Wait 300ms and then print
// "resolve success 1"
// Wait 300ms and then print
// "resolve success 2"
```

# Full version

You can find the full implementation here
- [MyPromise class](https://github.com/tmtxt/promise-from-scratch/blob/master/src/promise.ts)

You can also find all the test cases here
- [Resolve immediately](https://github.com/tmtxt/promise-from-scratch/blob/master/src/resolve-immediately.ts)
- [Basic then support](https://github.com/tmtxt/promise-from-scratch/blob/master/src/basic-then.ts)
- [Basic chaining](https://github.com/tmtxt/promise-from-scratch/blob/master/src/chaining-1.ts)
- [Chaining with Reject 1](https://github.com/tmtxt/promise-from-scratch/blob/master/src/chaining-2.ts)
- [Chaining with Reject 2](https://github.com/tmtxt/promise-from-scratch/blob/master/src/chaining-3.ts)
- [Throw error](https://github.com/tmtxt/promise-from-scratch/blob/master/src/chaining-4.ts)
- [Chaining with Promise](https://github.com/tmtxt/promise-from-scratch/blob/master/src/chaining-5.ts)

Or simply clone the full repo [here](https://github.com/tmtxt/promise-from-scratch)
