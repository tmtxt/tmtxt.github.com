---
layout: page
title: "Conkeror Extended Facebook Mode - CEFM"
description: "A Conkeror extension for ease of Facebook browsing"
group: project
---
{% include JB/setup %}

An extension for Conkeror's Facebook mode to help browse Facebook faster. It
extends the build-in Facebook mode to provide more commands to interactive with
Facebook News Feed, Story, Comment, Like,...

# Fallthrough keys for Facebook shortcuts

This extension lets you use some Facebook's shortcut keys for navigating on the
site.

- **/** - Search
- **Enter** - Focus on the text box when making a post
- **c** - Comment when on a story in News Feed
- **j** and **k** - Scroll between News Feed stories
- **l** - like or unlike a selected story
- **m** - New Message
- **q** - search for a friend to chat with

For the other keys, from 0-9, I haven't found the problem that prevents me from
unbind those keys from Conkeror.

# Open News Feed story in new buffer

When you browsing the News Feed with **j** and **k**, there is a little vertical
bar to indicate which story you're in. Simply invoke
**facebook-open-current-story-new-buffer** to open that story in new buffer or
**facebook-open-current-story-new-buffer-background** to open it in a new
background buffer. You can bind these two commands to some keystrokes for quick
navigation.

{% highlight js %}
define_key(facebook_keymap, "C-M-o", "facebook-open-current-story-new-buffer");
define_key(facebook_keymap, "C-S-o", "facebook-open-current-story-new-buffer-background");
{% endhighlight %}

<p>&nbsp;</p>

![Indicator](/files/conkeror-extended-facebook-mode/indicator.png)

# Other features

This is the very first version of the library so of course it doesn't have much
features. However, I'll add more later
