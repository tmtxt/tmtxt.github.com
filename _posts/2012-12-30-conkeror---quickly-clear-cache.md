---
layout: post
title: "Conkeror - Quickly clear cache"
description: ""
category: 
tags: [conkeror, cache, clear]
---
{% include JB/setup %}

Those of you who have been using Conkeror will soon realize that everytime you
want to clear cache, we have to eval a built-in (not interactive function)
called <span><b>cache_clear()</b></span>.
It takes us a lot of time especially when we develop our
website, everytime we want to see the changes, we have to type M-: and then type
in <span><b>cache_clear(CACHE_ALL)</b></span> and then hit return. This little code
below can help us quickly clear browser cache with just one key combination. Put
them into your **.conkerorrc**

Interactive function to clear all cache

{% highlight javascript %}
/// clear cache function
interactive("tmtxt-cache-clear-all", "clear all cache",
            function (I) {
			  cache_clear(CACHE_ALL);
            });
{% endhighlight %}

Next bind the above function to a key combination that you want, here I bind it
to <span><b>C-`</b></span>

{% highlight javascript %}
define_key(default_global_keymap, "C-`", "tmtxt-cache-clear-all");
{% endhighlight %}

Done! From now, just hit <span><b>C-`</b></span> to clear all cache of the browser.
