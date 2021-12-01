---
layout: post
title: "Async/Await in C# and Javascript"
description: ""
categories: [.net,javascript]
tags: []
thumbnail: /files/2018-11-15-async-await-in-c-and-javascript/async-await.png
---

[task]: https://docs.microsoft.com/en-us/dotnet/api/system.threading.tasks.task?view=netframework-4.7.2
[generator]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators
[promise]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
[taskrun]: https://docs.microsoft.com/en-us/dotnet/api/system.threading.tasks.task.run?view=netframework-4.7.2
[tpl]: https://docs.microsoft.com/en-us/dotnet/standard/parallel-programming/tpl-and-traditional-async-programming
[cojs]: https://www.npmjs.com/package/co
[jscsp]: https://www.npmjs.com/package/js-csp
[dotnetcore]: https://www.microsoft.com/net/download
[rider]: https://www.jetbrains.com/rider/
[visualstudio]: https://visualstudio.microsoft.com/vs/mac/
[workerthread]: https://nodejs.org/api/worker_threads.html

I recently changed from the **NodeJS** team to work in the **.Net** team (in the same company). Coming back
to C# after a long time, there are a lot of new stuffs. Actually, I used to hate **.Net** (simply
because I hate using Windows :LOL:). But thing has changed. [.Net Core][dotnetcore] can now run
on non-Windows systems without any differences. It is becoming easier to develop **.Net**
applications on Mac/Linux (using [Jetbrains Rider][rider] like me or
[Visual Studio Community][visualstudio] for Mac, which is a bad idea).

One interesting thing that I found in C# after a long time working in JS is
the **Async/Await** operation, which simplifies asynchronous programming significantly. I heard that
JS borrows the **Async/Await** idea from C#, so I decided to take a deeper look at the
**Async/Await** operation in C# and compare it to the one in JS to see if there are any other things
that C# is more successful at. There may be things that I was wrong about because I'm relatively new
to C#.

Below is the comparison table between using **Async/Await** pattern in C# and JS. I also mentioned JS
[Generator][generator] because it can be applied pretty much in the same way as the one using
[Promise][promise]. Actually, it used to be an innovative way to solve asynchronous problems in JS
before the birth of **Async/Await**. Many teams and products are still using it as the code base was
developed many years ago. Today, **Async/Await** is the preferred way for handling
asynchronous tasks in JS, leaving **Generator** back to its original purpose.

<!-- more -->

|                        | C#                                                                       | JS (yield)                                                             | JS (Async/Await)                                                         |
|------------------------|--------------------------------------------------------------------------|------------------------------------------------------------------------|--------------------------------------------------------------------------|
| **Traditional Method** | [TPL][tpl] - Callbacks                                                   | Callbacks                                                              | Callbacks                                                                |
| **Underlying**         | [Task][task]                                                             | [Generator][generator]                                                 | [Promise][promise]                                                       |
| **Invoke**             | Immediately                                                              | When calling **yield**                                                 | Immediately                                                              |
|                        | The operation is executed immediately after calling the **async** method | Calling a generator function returns a generator object only           | The operation is executed immediately after calling the **async** method |
| **Postpone Execution** | Wrap in a Lambda expression                                              | No need                                                                | Wrap in a function                                                       |
| **Use Cases**          | I/O & CPU-bound tasks                                                    | I/O                                                                    | I/O                                                                      |
| **Multithread**        | Use [Task.run][taskrun]                                                  | NO                                                                     | NO                                                                       |
|                        |                                                                          | JS is single-threaded, **Generator** doesn't help with CPU-bound tasks | JS is single-threaded, **Promise** doesn't help with CPU-bound tasks     |
| **Support**            | Native, C# 5+                                                            | Native, but not all environments                                       | Native (newer JS versions)                                               |
|                        |                                                                          | Requires 3rd-party libraries for async programming                     | Use polyfill libraries in old JS environment                             |
|                        |                                                                          | [co.js][cojs], [js-csp][jscsp]                                         |                                                                          |
{: .table }

Both the languages use **Async/Await** to solve a traditional problem with asynchronous programming,
that is how to express the async tasks in a expressive synchronous style. The traditional methods
use callbacks to handle the async tasks, which leads to a hard-to-maintain and
hard-to-debug code base. The code is also spread into different contexts, which is error-prone and
hard to reason about what is going on in the background. With **Async/Await**, the compiler (C#) or
the run time platform (JS) take care of the hard work, leaving your application with a logical
structure that resembles synchronous code.

C#, of course, is more powerful than JS in term of multi-threading support. It is not a surprise
that C# supports both the I/O operations and the heavy CPU jobs while JS is only capable
of I/O tasks. JS, by its nature, follows the single-threaded concurrency model, which relies on
just one single main thread for doing all the work. JS is not suitable for running CPU intensive
applications so **Generator** and **Promise** only provide a better way to organise the code. They
don't contribute anything to the performance.

There are still some other C# features that I haven't taken a deeper look at and waiting for me to
explore. It's quite an interesting environment, from the platform, the language to the support and
the tools (although I'm a bit allergic to its Capitalized style). There will be more posts like this
if I have enough time :D

**Update**: as the time of writing this post, NodeJS has started to experiment [Worker Thread][workerthread], a module that provides a way to create multiple environments running on independent threads. Many browsers also come with Service Worker APIs, which have the similar abilities as **Worker Thread**.
