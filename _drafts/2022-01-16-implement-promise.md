---
layout: post
title: "Implement Promise"
description: ""
categories: []
tags: []
thumbnail:
---

Just an interview question

> The interview question: Re-implement Javascript Promise from scratch. The implementation should
> support **chaining** (of course, asynchronously).

# Promise API

> It has been too long since I started using `async`/`await`. I almost forgot these Promise APIs.

Let's revisit the basic usage of Javascript
[Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise).

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

# Very basic implementation

Here is the very basic implementation with just `resolve` and `reject` support

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
```

Basic implement

```javascript
"use strict";

class MyPromise {
  constructor(executor) {
    const resolve = (res) => {
      console.log(`resolved: ${res}`);
    };
    const reject = (err) => {
      console.log(`reject: ${err}`);
    };

    try {
      executor(resolve, reject);
    } catch (e) {
      reject(e);
    }
  }
}

const myPromise = new MyPromise((resolve, reject) => {
  setTimeout(() => {
    resolve("foo");
  }, 300);
});
```

Basic `then`

```javascript
'use strict';

class MyPromise {
  status = 'init'; // init|resolved|rejected
  res = null;

  constructor(executor) {
    const resolve = (res) => {
      if (this.status === 'init') {
        this.status = 'resolved';
        this.res = res;
      }
    };

    const reject = (err) => {
      if (this.status === 'init') {
        this.status = 'rejected';
        this.res = err;
      }
    };

    try {
      executor(resolve, reject);
    } catch (e) {
      reject(e);
    }
  }

  then(handleResolved, handleRejected) {
    if (this.status === 'resolved') {
      handleResolved(this.res);
    } else if (this.status === 'rejected') {
      handleRejected(this.res);
    }
  }
}

const myPromise = new MyPromise((resolve, reject) => {
  resolve('foo');
});

myPromise.then((value) => {
  console.log(value);
});
```

Basic async

```javascript
'use strict';

class MyPromise {
  status = 'init'; // init|resolved|rejected
  res = null;

  resolveFn = null;
  rejectFn = null;

  constructor(executor) {
    const resolve = (res) => {
      if (this.status === 'init') {
        this.status = 'resolved';
        this.res = res;

        this.resolveFn && this.resolveFn(this.res);
      }
    };

    const reject = (err) => {
      if (this.status === 'init') {
        this.status = 'rejected';
        this.res = err;

        this.rejectFn && this.rejectFn(this.res);
      }
    };

    try {
      executor(resolve, reject);
    } catch (e) {
      reject(e);
    }
  }

  then(handleResolved, handleRejected) {
    this.resolveFn = handleResolved;
    this.rejectFn = handleRejected;
  }
}

const myPromise = new MyPromise((resolve, reject) => {
  setTimeout(() => {
    resolve('foo');
  }, 500);
});

myPromise.then((value) => {
  console.log(value);
});
```
