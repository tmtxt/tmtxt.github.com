---
layout: post
title: "From Object-oriented to Functional programming - Part 1"
description: ""
categories: [misc]
tags: []
thumbnail:
---

Object-oriented programming (OOP) and functional programming (FP) are two popular programming
paradigms. In recent years, there has been a trend towards FP due to its benefits such as increased
reliability and easier testing. In this article, I'll show some helpful patterns for people coming
from the OOP world.

# Private properties

In object-oriented programming, a private property is a member variable that is only
accessible within the class in which it is defined. Private properties are often used to store
internal data for an object that should not be directly modified by external code. Private
properties do not exist in the same way in functional programming as they do in object-oriented
programming. To archive this, simply use a closure to wrap the private properties.

Here is a simple example about private properties in C#

```csharp
public class Controller
{
    private int _totalCount = 0;

    public void Increase(string user, int amount)
    {
        if (user != "truongtx")
            return;

        _totalCount += amount;
    }

    public int GetTotal()
    {
        return _totalCount;
    }
}

public class Runner
{
    public void Run()
    {
        var controller = new Controller();
        controller.Increase("truongtx", 100);
        controller.Increase("otherUser", 200);
        Console.Out.WriteLine(controller.GetTotal());
    }
}
```

And here is how you would do it the functional way...

<!-- more -->

```typescript
// Example in Typescript
interface Controller {
  increase(user: string, amount: number): void;
  getTotal(): number;
}

const createController = (): Controller => {
  let total = 0;

  return {
    increase: (user, amount) => {
      if (user !== 'truongtx') {
        return;
      }

      total += amount;
    },

    getTotal: () => total,
  };
};

// To initialize and run
const controller = createController();
controller.increase('truongtx', 100);
controller.increase('otherUser', 200);
console.log(controller.getTotal());
```

# To be continued

> This is also an experimental post, which I used [ChatpGPT](https://openai.com/blog/chatgpt/) to
> generate most of the text 😂
