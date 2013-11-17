---
layout: post
title: "Conkeror Extended Facebook Mode - Update new features"
description: ""
category: Conkeror
thumbnail: 
showtn: no
tags: [conkeror, cefm]
---
{% include JB/setup %}

Today, I have added my extension for Conkeror some new features. They are small
but really useful for interacting with Facebook effectively through the
keyboard.

You can find the information as well as the instruction on how to install and
use at the package's homepage
<http://truongtx.me/conkeror-extended-facebook-mode.html>

# Cycle through chat conversions with keyboard

Everything will be great if there is a handy keystroke
for cycling through chat conversations. CEFM provides a useful command named
**facebook-cycle-conversations** to help you solve this problem. Simply bind it
to a keystroke that you want, for example

{% highlight js %}
define_key(facebook_keymap, "C-C", "facebook-cycle-conversations");
{% endhighlight %}

Now in every facebook page, type **q** to search for a friend to chat, press
**C-C** to jump to the next chat conversation. Usually if you press **Esc**, the
current chat conversation will be closed. If you want to continue browsing
Facebook without closing it, you can see the steps how to achieve it in this
post [Using Esc key in Conkeror]({%post_url 2013-08-08-using-esc-key-in-conkeror%}).

<p align="center">
<img src="/files/conkeror-extended-facebook-mode/fbchat3.png" />
</p>

# Functions to fix fallthrough keys that Facebook cannot recognize

For keys from 0-9, there is a little problem so that I have to
implement some custom functions for performing their functionalities but have
not finished all, they will appear in the next version.

- **3** - Open Friend Requests panel
- **4** - Open Messages panel
- **5** - Open Notifications panel

<!-- more -->
