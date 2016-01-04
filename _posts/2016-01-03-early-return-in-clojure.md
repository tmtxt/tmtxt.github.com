---
layout: post
title: "Early Return in Clojure"
description: ""
categories: [misc]
tags: []
thumbnail: 
---
{% include JB/setup %}

> Okay  
> Okay  
> Okay...  
> It's better that you break your function into smaller ones, each does one
> simple purpose. Clojure is functional, isn't it?

I'm just kidding. Sometimes it's really hard to write such that code. Consider
this example, I have a function for validating whether a string is a valid date
time string. If it's nil or blank, just skip it, otherwise, try parsing it to
see if it's okay.

{% highlight cl %}
(defn validate-date-time [date-time]
  (if (nil? date-time) true
      (if (blank? date-time) true
          (try (f/parse formatter date-time)
               true
               (catch Exception e false)))))
{% endhighlight %}

Nested, nested and nested. If this is still simple and easy to see
for you, try this one, need to check if the date time is between 1970 and 2030

{% highlight cl %}
(defn- validate-date-time [date-time]
  (if (nil? date-time) true
      (if (blank? date-time) true
          (let [date-time (f/parse formatter date-time)]
            (if (nil? date-time) false
                (if (before-1970? date-time) false
                    (if (after-2030? date-time) false
                        true)))))))
{% endhighlight %}

Ehhh...

<!-- more -->

Okay, stop here! I don't want my head to be blown up by these nested
parentheses. This makes me remember the js callback hell days before
[Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
age. It's much better if we can write something like this, like what we usually
do in other programming languages

{% highlight js %}
function validateDateTime(dateTime) {
  if (dateTime === null) return true;
  if (dateTime === '') return true;

  dateTime = f.parse(formatter, dateTime);

  if (dateTime === null) return false;
  if (before1970(dateTime)) return false;
  if (after2030(dateTime)) return false;

  return true;
}
{% endhighlight %}

Hmm, much better huh? Unfortunately, we don't have anything called early return
in **Clojure**. We can do a little hack using `try`, `catch` but they support only
for throwing and catching exception.

Luckily, I found
[slingshot](https://github.com/scgilardi/slingshot), an
**Enhanced throw and catch for Clojure** that allows you to throw anything to
the catch block. What you need to do is just to wrap your code inside its try
and catch block and throw whatever value you want to return. The above evil code
can be transformed into this

{% highlight cl %}
(defn- validate-date-time [date-time]
  (try+
   ;; skip if nil or blank
   (when (nil? date-time) (throw+ true))
   (when (blank? date-time) (throw+ true))

   ;; try parsing it
   (let [date-time (f/parse formatter date-time)]
     ;; validate the result
     (when (nil? date-time) (throw+ false))
     (when (before-1970? date-time) (throw+ false))
     (when (after-2030? date-time) (throw+ false))
     true)
   (catch Object res res)))  ;; catch and return anything that is thrown
{% endhighlight %}

Phew! We still have one nested level at the `let` block but it's much better
compared to the original one. I think that's how **Clojure** do block scoping
and we have no other way to re-assign the variable so we cannot avoid that
nested level, but it's still better than writing the nested `if` blocks.

That's how I do early return in **Clojure**. Once again, thanks `slingshot` for
making this possible! :D
