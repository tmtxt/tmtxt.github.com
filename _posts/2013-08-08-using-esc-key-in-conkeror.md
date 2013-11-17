---
layout: post
title: "Using Esc key in Conkeror"
description: ""
category: conkeror
thumbnail: 
showtn: no
tags: [conkeror, web browser]
---
{% include JB/setup %}

In most browsers, the Esc key is used to stop the browser from loading a web
page. In some web pages, for example Facebook, the Esc key has other
functionality. When you click on an image on Facebook, another layer for displaying
that image will appear. If the Esc key is pressed, that layer will disappear and
you will be taken to the previous page. Similar things happen in some gallery
pages, too.

However, in Conkeror, when the Esc key is pressed, the browser will release the
focus on the currently focused element (if any) and after that move the focus to
the top frame. In some circumstances, it can cause problem when both the current
element will lost the focus and the web page will also execute the special
funationality bind to Esc while you want only one of them to be activated.

Look at this example below, you cursor is now inside a textbox. After typing
some text, you want to unfocus the current textbox and then follow the button
(Share Photo) to share that post to your timeline. But if you press Esc, the
dialog will disappear because Facebook also bind the Esc key to Close the dialog.

<!-- more -->

![Example](/files/2013-08-08-using-esc-key-in-conkeror/example.png)

To solve this problem, we need two Esc keys in Conkeror. Simply define an alias
to the Esc key in the .conkerorrc file, for example

{% highlight js %}
define_key_alias("C-o", "escape");
{% endhighlight %}

Now when you want Conkeror to unfocus the current element only, just press the
alias key C-o. Pressing Esc key will cause Conkeror to activate the page's key
binding for Esc.
