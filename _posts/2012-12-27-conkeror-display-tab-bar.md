---
layout: post
showtn: 
title: "Conkeror - Display Tab bar"
description: ""
category: conkeror
thumbnail: /files/2012-12-27-conkeror-display-tab-bar/tab.png
tags: [conkeror, emacs, browser]
---
{% include JB/setup %}

> **Note**: for those of you who don't know what Conkeror is, please read this
> post
> [Conkeror and How it changed the way I surf the web](/2012/12/24/conkeror-and-how-it-changed-the-way-i-surf-the-web/).
> If you don't know about the conkeror rc file, please read this post
> [Conkeror Getting Started](/2012/12/25/conkeror---getting-started---part-2/)

If this is the first time you've been to Conkeror, you will find it a bit
difficult to approach. It does not have the full GUI like the other browsers.
Everytime you want to switch to the other buffer, you have to type **C-x b**,
which is very hard for you to imagine which and how many web pages are currently
opened. In this post, I'll show you how to make Conkeror display the tab bar.

![Conkeror Tab bar](/files/2012-12-27-conkeror-display-tab-bar/tab.png)

<!-- more -->

Conkeror have the built-in tab feature but it's not enable by default. To
activate it, simply put this into your .conkerorrc

{% highlight js %}
require("favicon.js");
require("new-tabs.js");
tab_bar_show_icon = true;
tab_bar_show_index = true;
{% endhighlight %}

-----

**Conkeror Homepage**: <http://conkeror.org/>  
**My conkeror on github**: <https://github.com/tmtxt/conkerorrc>  
*Follow me and we can exchange the experience.*  
