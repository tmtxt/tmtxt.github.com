---
layout: post
title: "Conkeror - Stop Loading and Reload all buffers"
description: ""
category: Conkeror Web Browser
thumbnail: /files/2012-12-24-conkeror-getting-started-part-1/conkeror-main.png
tags: [tutorial, conkeror, buffers]
---
{% include JB/setup %}

I usually open many buffers in conkeror (50-100 buffers). Also, I have auto save
mode enable so everytime I start Conkeror, it automatically loads all of those
buffers. That can cause my network overload. Moreover, sometimes I need reload
all the currently opend buffers to see all the changes. So I wrote those 2
little pieces of code to help me archive it. Here are they:

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
