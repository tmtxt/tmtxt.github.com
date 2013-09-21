---
layout: page
title: "Conkeror Extended Facebook Mode - CEFM"
description: "A Conkeror extension for ease of Facebook browsing"
group: project
---
{% include JB/setup %}

# Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Facebook's default keyboard shortcuts](#facebooks_default_keyboard_shortcuts)
- [Open News Feed story in new buffer](#open_news_feed_story_in_new_buffer)
- [Cycle through chat conversions with keyboard](#cycle_through_chat_conversations_with_keyboard)

# Introduction

An extension for Conkeror's Facebook mode to help browse Facebook faster. It
extends the built-in Facebook mode to provide more commands to interact with
Facebook News Feed, Story, Comment, Like,...

# Installation

Clone the repo on Github at
[CEFM Repo](https://github.com/tmtxt/conkeror-extended-facebook-mode) and put it
inside your .conkerorrc folder. After that, add this to your init file

{% highlight js %}
let (path = get_home_directory()) {
  // add to load path
  path.appendRelativePath(".conkerorrc");
  path.appendRelativePath("conkeror-extended-facebook-mode");
  load_paths.unshift(make_uri(path).spec);

  // include the library
  require("conkeror-extended-facebook-mode.js");  
};
{% endhighlight %}

# Facebook's default keyboard shortcuts

This extension lets you use some Facebook's shortcut keys for navigating on the
site.

- **/** - Search
- **Enter** - Make comment or Focus on the text box when making a post
- **j** and **k** - Scroll between News Feed stories
- **l** - like or unlike a selected story
- **m** - New Message
- **q** - search for a friend to chat with

For the other keys, from 0-9, there is a little problem so that I have to
implement some custom functions for performing their functionalities but have
not finished all.

- **3** - Open Friend Requests panel
- **4** - Open Messages panel
- **5** - Open Notifications panel

# Open News Feed story in new buffer

When you browsing the News Feed with **j** and **k**, there is a little vertical
bar (old-style news feed) or a thin border around the avatar image of the story
to indicate which story you're in. Simply invoke
**facebook-open-current-story-new-buffer** to open that story in new buffer or
**facebook-open-current-story-new-buffer-background** to open it in a new
background buffer.

<p align="center">
<img src="/files/conkeror-extended-facebook-mode/indicator.png" />
</p>

<p align="center">
<img src="/files/conkeror-extended-facebook-mode/indicator-new.png" />
</p>

You can also bind these two commands to some keystrokes for quick
navigation.

{% highlight js %}
define_key(facebook_keymap, "C-M-o", "facebook-open-current-story-new-buffer");
define_key(facebook_keymap, "C-S-o", "facebook-open-current-story-new-buffer-background");
{% endhighlight %}

# Cycle through chat conversions with keyboard

Facebook chat is very convenient but can be very painful, too. Facebook page
contains too many elements, from links on sidebar, news
feed,... to the textboxes in status update, chat conversations,... This will
slow down your productivity if you're Facebook-addicted (like me) since Conkeror
have to process too many hint links and you also have to look for the right hint
number and type exactly that number (can be up to 3 digits) if you want to chat.
Moreover, if you usually chat with many people at the same time, this can become
a nightmare. You have to follow the textbox (with 3 digits hinting number) for
chatting with the first person and then unfocus it, follow again another textbox
for the next person,... For easy imagination, see the picture below

<p align="center">
<img src="/files/conkeror-extended-facebook-mode/fbchat1.png" />
</p>

<p align="center">
<img src="/files/conkeror-extended-facebook-mode/fbchat2.png" />
</p>

Everything will be great if there is a handy keystroke
for cycling through the chat conversations. **CEFM** provides a useful command named
**facebook-cycle-conversations** to help you solve this problem. Simply bind it
to a keystroke that you want, for example

{% highlight js %}
define_key(facebook_keymap, "C-C", "facebook-cycle-conversations");
{% endhighlight %}

Now in every facebook page, type **q** to search for a friend to chat, press
**C-C** to jump to the next chat conversation. Usually when you press **Esc**, the
current chat conversation will be closed. If you want to continue browsing
Facebook without closing it, you can see the steps how to achieve it in this
post [Using Esc key in Conkeror]({%post_url 2013-08-08-using-esc-key-in-conkeror%}).

<p align="center">
<img src="/files/conkeror-extended-facebook-mode/fbchat3.png" />
</p>

# Other features

This is just the very first version so of course it doesn't have much
features. However, I'll add more later
