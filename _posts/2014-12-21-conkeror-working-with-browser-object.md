---
layout: post
title: "Conkeror - Working with Browser Object"
description: ""
categories: [conkeror]
tags: []
---
{% include JB/setup %}

> If you have not known what Conkeror is, you probably miss one of the greatest
> web browser in your life. Read this post for an introduction of how impressive
> Conkeror is
> [Conkeror and How it changed the way I surf the web]({%post_url 2012-12-24-conkeror-and-how-it-changed-the-way-i-surf-the-web%}).

# Overview

One of the most interesting feature that Conkeror brings from Emacs is the
[Browser Object](http://conkeror.org/BrowserObjects). If you are coming from an
Emacs background, you are not unfamiliar with universal-arguments, which can
modify the function's behaviour once it is called before activating the
function. **Browser Objects** in Conkeror brings that idea from Emacs to the web
environment but with better approach. **Browser Objects** in Conkeror can
represent practically any type of data (e.g image, hyperlinks, frame,...).

**Quote from Conkeror Wiki page**

> In the web environment, there are many data types which one wants to
> manipulate. Text, hyperlinks, images, form fields, to name just a few. Without
> some kind of abstraction layer to unify the interface to all these types of
> objects, it would be necessary to have a combinatorially large number of
> commands. For instance, you would need separate commands for follow-link,
> follow-image, and follow-frame. With browser objects, you have just one
> command, follow, which can operate on many types of data.

<!-- more -->

# Using Browser Objects Commands

Browser Objects commands are prefix commands and they have the attributes just
like normal command in Conkeror. You can activate them with `M-x`, you can
bind them to a key stroke with `define_key`. All of them are named with a prefix
`browser-object-`. Once activated, they will change the interactive context of
the next command invoked.

For example, the `save` command (which is default bound to `s`), prompts for a
link to save to your computer. Now, try to modify it using the
`browser-object-images` command to make it prompt for images to download.
The command is bound to `i` be default so you can just type `i s` instead of
just `s` to select the image you want to download. Of course you can active it
with `M-x` `browser-object-images` `M-x` `save`.

![Alt Text](/files/2014-12-21-conkeror-working-with-browser-object/image.png)

# Some useful tips

Conkeror already comes pre-built with many browser objects commands that you can
use

## Images

- `i f` - `browser-object-images` `follow`: follow image
- `i s` - `browser-object-images` `save`: save image

## Copy

- `* * c` - `browser-object-dom-node` `copy`: copy DOM node text (very useful to
  copy block of text)
- `i c` - `browser-object-images` `copy`: copy image link
- `* a c` - `browser-object-alt` `copy`: copy image alt text

There are many other browser objects that are waiting for you to explore

# Defining Browser Objects Class and Commands

The Browser Object commands are generated automatically by Conkeror when you
create the Browser Object class. The function `define_browser_object_class` is
used to define the Browser Object class. You need to supply it at least 3
arguments, these are the name of the browser object (in hyphenated style), the
doc string of it and the handler (will talk later). The final optional argument
is a keyword using for the prompt text.

First, let look at an example and then I will explain it more clearly later. This
example is used for selecting the news link on
[Hacker News](https://news.ycombinator.com/).

{% highlight js %}
define_browser_object_class("yc-links",
                            "yc news link",
                            xpath_browser_object_handler("//td[@class='title']/a"),
                            $hint = "select link");
{% endhighlight %}

And now, evaludate it or restart Conkeror for it to take effect. Open
[Hacker News](https://news.ycombinator.com/), press `M-x`
`browser-object-yc-links` and then press `f` (`follow`) and magically this is
what you get. Conkeror will prompt for only the news links on the Hacker News
page, not all the links exist on the page.

![Alt Text](/files/2014-12-21-conkeror-working-with-browser-object/yc.png)

It will not be magical any more after I explain the meaning of the above code.
First, let take a look at the DOM structure of the YCombinator page. The links
for the news are located inside a table cell like this

{% highlight html %}
...
<td class="title">
  <a href="http://paulgraham.com/ecw.html">How to Be an Expert in a Changing
    World
  </a>
  <span class="comhead"> (paulgraham.com)
  </span>
</td>
...
<td class="title">
  <a href="https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=773619">Page load
    fail makes it difficult to cook cornbread in the woodstove
  </a>
  <span class="comhead"> (debian.org)
  </span>
</td>
...
{% endhighlight %}

Can you see any similarity? All the links are located inside a `<td>` with class
**title**. So how to retrieve them? Conkeror offers a function named
`xpath_browser_object_handler` that receives an xpath expression and returns the
handler function that can be used for `define_browser_object_class`. If you
don't know what XPath is yet, you can easily read about it and find examples on
Google. The XPath expression for retriving all the `<a>` inside `<td
class="title">` is `//td[@class='title']/a`. Now, we have everything we need to
create a Browser Object class, I will repeat the sample code here

{% highlight js %}
define_browser_object_class("yc-links",
                            "yc news link",
                            xpath_browser_object_handler("//td[@class='title']/a"),
                            $hint = "select link");
{% endhighlight %}

The first argument is the name of the Browser Object class. From this, Conkeror
will generate an interactive command named `browser-object-yc-links`.

The second argument is just a doc string for this Browser Object

The third argument is where the magic happens as I have discussed before.

The last argument is the text used for prompting in the minibuffer (you can see
in the image above).

# Default Browser Object for command

Instead of having to activate two commands (browser object command and normal
command), you can define your own command that uses an existing command but with
another browser object as the default

{% highlight js %}
interactive("follow-yc-links",
            "follow the news link on yc",
            "follow",
            $browser_object = browser_object_yc_links);
{% endhighlight %}

This interactive command calls the `follow` command but will prompt for the yc
news links only.

# Last thing...

You can use Browser Object feature to define any kind of component in the web
page that you want to interact without having to re-define the same command for
every different data type.

However, I don't like the XPath expression very much. It would be much better if
I can use CSS Selector (maybe it's already implemented somewhere in Conkeror
source code but I have not read that part). When I discover it or implemented it
successfully, I will come back to update this post.
