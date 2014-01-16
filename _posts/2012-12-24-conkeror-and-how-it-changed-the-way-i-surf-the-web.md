---
layout: post
showtn: yes
title: "Conkeror and How it changed the way I surf the web"
description: ""
category: conkeror
tags: [conkeror, emacs, web browser]
---
{% include JB/setup %}

For those of you who haven't known about Conkeror yet, here is a brief description about it:

> Conkeror is a keyboard-oriented, highly-customizable, highly-extensible web
> browser based on Mozilla XULRunner, written mainly in JavaScript, and
> inspired by exceptional software such as Emacs and vi. Conkeror features a
> sophisticated keyboard system, allowing users to run commands and interact
> with content in powerful and novel ways.  It is self-documenting, featuring
> a powerful interactive help system.

The first time I heard about conkeror was in 2010. And now it becomes my daily used application. Though I still use many other web browsers (Chrome, Firefox, Safari and Opera), Conkeror is my main web browser and usually the first app I open when I start my computer. Here are some key features that make me really impressed in Conkeror.

<!-- more -->

# Keyboard-oriented

Yes as I mentioned above in the description, it's a keyboard-oriented browser. This is the most impressing feature of Conkeror. I love conkeror right at the first time I saw my cousin used it. I knew I had found the right browser for me. Most of the time you don't need to move your hand out of the keyboard, or even out the home row area. This can speed up the way we use our computer many times.

One example is in the photo below. This is how conkeror follow a link in the web page.
![Keyboard-Oriented](/files/2012-12-24-conkeror-and-how-it-changed-the-way-i-surf-the-web/keyboard-oriented.png)

Of course, there are much more things we can do with conkeror using the keyboard, eg saving images, downloading file, jumping to specific tab, running command, copying a specific block of text, interact with form,...

# Simplicity and Less Resource Consuming

![Simplicity](/files/2012-12-24-conkeror-and-how-it-changed-the-way-i-surf-the-web/simplicity.png )

Yes! As you can see in the picture above, it's simple, extremely simple. Simplicity makes it different from other web browser. Despite of the difficulty for beginner, once you get used to it, you will feel extrememely comfortable. There are no button, no address bar, nearly nothing but the web page so we can easily concentrate on the content of the website. Also it is highly appropriate for modern computer with the wide screen, maximize the web page display area. After a long time get used to the animated effects, I come back to the simplest thing.

Simplicity makes conkeror consume far less resource than its relative, Firefox, and even Google Chrome. In the picture above, usually I open more than 50 tabs, and in many cases, my web browser holds more than 100 tabs. Everything still runs smoothly with many other application opened on my 4GB of Ram Core 2 Duo Hackintosh or 4GB of Ram Macbook (now I have upgraded it to 16GB of Ram :LOL: ). I heard somebody using ubuntu linux said that conkeror even runs faster and consumes less resource on their computer. I haven't tested it yet but for me this is highly adequate.

# Highly customizable and Highly-extensible

Like any other open source software, conkeror users can be the developers, too. We have a big community who use this application.

We can easily customize all the key bindings based on our favor. And like many other unix applications, it also has the rc file so just put everything you need in that rc file and customize conkeror as you like. You can look up on the conkeror site and the wiki and then add any features that the community contribute to or you can implement some new features by yourself.

{% highlight js %}
// next and previous buffer
define_key(default_global_keymap, "A-z", "buffer-previous"); //one hand user
define_key(default_global_keymap, "C-j", "buffer-previous"); //two hands user
define_key(default_global_keymap, "A-x", "buffer-next"); //one hand user
define_key(default_global_keymap, "C-l", "buffer-next"); //two hands user
{% endhighlight %}
One example the rc file in rebinding keys for conkeror.

**There are still many other awesome features in conkeror.**

Conkeror Homepage: <http://conkeror.org/>  
My conkeror on github: <https://github.com/tmtxt/conkerorrc>  
*Follow me and we can exchange the experience.*

I also posted this article on the website of RMIT Vietnam IT Club  
Link: <http://rmitc.org/2012/12/conkeror-and-how-it-changed-the-way-i-surf-the-web/>
