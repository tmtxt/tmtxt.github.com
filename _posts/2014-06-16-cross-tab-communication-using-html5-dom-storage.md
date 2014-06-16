---
layout: post
title: "HTML5 Web Storage and Cross-tab communication"
description: ""
categories: [javascript]
tags: []
---
{% include JB/setup %}

# HTML5 Web Storage

> HTML Web Storage is a way for web pages to store named key/value pairs locally,
> within the client web browser. Like cookies, this data persists even after you
> navigate away from the web site, close your browser tab, exit your browser, or
> what have you. Unlike cookies, this data is never transmitted to the remote web
> server (unless you go out of your way to send it manually).
> [HTML5 Storage](http://diveintohtml5.info/storage.html)

There are 2 types of HTML5 Web Storage, Session Storage and Local Storage. Both
of them exist as properties of `window` object and can be accessed as
`sessionStorage` and `localStorage`. The methods of them are similar, too. The
only difference is that objects stored in session storage will be cleared when
the session expires while the ones stored in local storage are not. The
same-origin rules is applied for both of them.

To store an object in the Web Storage location, use `setItem(key,value)`.

{% highlight js %}
// Store in session
sessionStorage.setItem("username", "tmtxt");

// Persist the data
localStorage.setItem("username", "tmtxt");

// Use JSON.stringify() to store JSON data
sessionStorage.setItem("json", JSON.stringify(myjson));
{% endhighlight %}

<!-- more -->

To retrieve the data

{% highlight js %}
// Get from session storage
sessionStorage.getIem("username"); 

// Get from local storage
localStorage.getIem("username");

// Get the JSON data
JSON.parse(sessionStorage.getIem("username"));
{% endhighlight %}

# Cross-tab Communication with HTML5 Storage

The **localStorage** object comes with **storage** event for tracking changes to
the HTML5 storage area. The event is fired whenever `setItem()` or
`removeItem()` functions of **localStorage** is called. By registering to this
event, multiple browser tabs displaying pages from the same origin can
communicate easily every time the data is changed.

{% highlight js %}
if (window.addEventListener) {
  // Normal browsers
  window.addEventListener("storage", handler, false);
} else {
  // for IE (why make your life more difficult)
  window.attachEvent("onstorage", handler);
};

function handler(e) {
  console.log('Successfully communicate with other tab');
  console.log('Received data: ' + localStorage.getItem('data'));
}
{% endhighlight %}

Now, run a simple web server and open many tabs to that page, try writing data to
local storage using Javascript console and see the reaction in other tab

{% highlight js %}
localStorage.setItem('data', 'hello world');
{% endhighlight %}

![Cross-tab](/files/2014-06-16-cross-tab-communication-using-html5-dom-storage/cross-tab.gif)
