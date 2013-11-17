---
layout: post
title: "Conkeror Extended Facebook Mode - New update for scrolling chat conversation"
description: ""
category: conkeror
thumbnail: 
showtn: no
tags: [cefm, conkeror]
---
{% include JB/setup %}

I've just implemented new feature for CEFM - Scrolling chat conversation.

Usually, while you are chatting with your friend, you want to scroll up the
conversation for viewing the conversation history. The command
**facebook-scroll-up-current-coversation** and
**facebook-scroll-down-current-coversation** will help you easily scroll through
the current chat conversation that you are in. Simply bind it to any key stroke
that you want to use, for example

{% highlight js %}
define_key(facebook_keymap, "C-I", "facebook-scroll-up-current-coversation");
define_key(facebook_keymap, "C-K", "facebook-scroll-down-current-coversation");
{% endhighlight %}

You can also config the scroll gap (the distance for each scroll) by setting the
following variable

{% highlight js %}
facebook_mode_scroll_gap = 50;
{% endhighlight %}

For more information about the mode, visit its homepage at
<http://truongtx.me/conkeror-extended-facebook-mode.html>

<!-- more -->
