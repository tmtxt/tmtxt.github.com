---
layout: post
title: "async/await in C# and Javascript"
description: ""
categories: []
tags: []
thumbnail:
---

[task]: https://docs.microsoft.com/en-us/dotnet/api/system.threading.tasks.task?view=netframework-4.7.2
[generator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators
[promise]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise

# Traditional Asynchronous Programming

## Javascript

### Callbacks and Callback Hell

First, let's take a look at a simple example

```js
// async tasks, which send API requests to other server
function login(onDone, onError) {...}
function getUserInfo(onDone, onError) {...}
function getTodayEvents(onDone, onError) {...}

// first you need to login to perform any action
login(
  function(token) {
    // get current user info
    getUserInfo(
      function(user) {
        // get all today events for this user
        getTodayEvents(
          function(events) {
            // handle your logic here
          },
          function(err) { /* handle get events error */ }
        );
      },
      function(err) { /* handle get user info error here */ }
    );
  },
  function(err) { /* handle login error here */ }
);

console.log("Hello I'm done");

```

Coming from other language, you may be surprised since everything in JS is designed in an
asynchronous way. The last `console.log` command is executed before all the other operations finish.
This is one of the most confusing thing for new JS developers (including me in the past).

Traditional JS programming uses callback to handle asynchronous tasks, like File I/O, sending
API requests to server,... The callback function is a handler function which will be invoked at some
point of time in the future when the async action finishes its job. Following this pattern, you will
soon end up with a large and ugly pyramid of callbacks, which is called Callback Hell. For more
information about the callback hell, check this <http://callbackhell.com/>.

# Async/Await

|                | C#                                                                       | JS (yield)                                                   | JS (Async/Await)                                                         |
|----------------|--------------------------------------------------------------------------|--------------------------------------------------------------|--------------------------------------------------------------------------|
| **Underlying** | [Task][task]                                                             | [Generator][generator]                                       | [Promise][promise]                                                       |
| **Invoke**     | Immediately                                                              | When calling **yield**                                       | Immediately                                                              |
|                | The operation is executed immediately after calling the **async** method | Calling a generator function returns a generator object only | The operation is executed immediately after calling the **async** method |
| **Use Cases**  | I/O & CPU-bound tasks                                                    | I/O                                                          | I/O                                                                      |
|                |                                                                          | The nature of JS (single-threaded)                           |                                                                          |
|                |                                                                          |                                                              |                                                                          |
{: .table }
