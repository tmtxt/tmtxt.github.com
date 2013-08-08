---
layout: post
showtn: 
title: "Conkeror - Stop Loading and Reload all buffers"
description: ""
category: conkeror
thumbnail: /files/2012-12-24-conkeror-getting-started-part-1/conkeror-main.png
tags: [conkeror, emacs, web browser]
---
{% include JB/setup %}

I usually open many buffers in conkeror (50-100 buffers). Also, I have auto save
mode enable so everytime I start Conkeror, it automatically loads all of those
buffers. That can cause my network overloaded. Moreover, sometimes I need reload
all the currently opend buffers to see all the changes. So I wrote those 2
little pieces of code to help me achieve it. Here are they:

## Stop loading all buffer (key A-h)

{% highlight javascript %}
define_key(default_global_keymap, "A-h",
          function (I)
          {
              for (var i = 0; i < I.window.buffers.count; i++)
              {
                  stop_loading(I.window.buffers.get_buffer(i));
              }
          });
{% endhighlight %}

<!-- more -->

## Reload all buffer (key A-r)

{% highlight javascript %}
define_key(default_global_keymap, "A-r",
          function (I)
          {
              for (var i = 0; i < I.window.buffers.count; i++)
              {
                  reload(I.window.buffers.get_buffer(i));
              }
          });
{% endhighlight %}

