---
layout: post
showtn: 
title: "Conkeror - Getting Started - Part 2"
description: ""
category: conkeror
tags: [conkeror, emacs, web browser]
---
{% include JB/setup %}

> Learn Conkeror by using Conkeror ;)

The first time you open Conkeror, it will show you the tutorial. It is the basic thing you need to know to use Conkeror. Moreover, in Conkeror, you can even view all the lists of functions, key bindings, what this key combination do,... So if you get stuck, just open them. Here I will show you how.

![Conkeror Tutorial](/files/2012-12-25-conkeror---getting-started---part-2/tutorial.png)

<!-- more -->

# The RC file

Like many other Unix-based application, conkeror also has its own rc file. The rc file is your own script and is auto loaded when conkeror starts. The rc file location is $HOME/.conkerorrc where $HOME is your home directory. The .conkerorrc can be a text file written in javascript or a directory. If it's a directory, Conkeror will loads all the files with the extension .js in that directory. If you want to have your own config, just put them into the rc file.

# Understanding the terms and notation

The first thing you need to know is the key notation. Conkeror is built based on emacs style so it uses modifier keys nearly all the time. There are some basic modifier keys in Conkeror, they are:  
* Control (C-)
* Shift (S-)
* Alt (A-)
* Meta (M-)

Control and Shift key are popular to you. The special is the Alt key and Meta key. On OSX, the command key is the Meta key, the option key is the Alt key. In other OS, usually, the alt key on your keyboard is considered as Meta key, the windows key on your keyboard is consider as Alt key.

*Note*: on OSX, the option+key produces a dead key, which means you cannot use it as a modifier key in conkeror. To fix it, download this file USExtendedNoDeadKey.keylayout from this [link](http://conkeror.org/Keyboard?action=AttachFile&do=view&target=USExtendedNoDeadKey.keylayout) or this [link](/files/2012-12-25-conkeror---getting-started---part-2/USExtendedNoDeadKey.keylayout) and put it into ~/Library/Keyboard Layouts/. Log out and log in back. Open System Preferences > Language & Text > Input Sources, select the layout US (NDK). After that, put this into your conkerorrc file

{% highlight javascript %}
modifiers.M = new modifier(function (event) { return event.altKey; },
                           function (event) { event.altKey = true; });
modifiers.A = new modifier(function (event) { return event.metaKey; },
                           function (event) { event.metaKey = true; });
{% endhighlight %}

Now you understand the modifier notation in conkeror. If a key binding is "C-n", it means "Ctrl + n", "C-h t" means "Ctrl + h and then t", "C-x C-f" means "Ctrl + x and Ctrl + f".

# Some useful key bindings

    C-g : abort the what is currently running (page loading, command, key sequence)  
	C-h t : show the conkeror tutorial  
	C-h k : discribe what a key binding does  
	C-h f : discribe function  
	C-h b : show all key bindings

#### You're ready! Now open up your conkeror and read the tutorial and start.

**Reference**:  
[RC file](http://conkeror.org/ConkerorRC)  
[Conkeror Keyboard](http://conkeror.org/Keyboard)  

Conkeror Homepage: <http://conkeror.org/>  
My conkeror on github: <https://github.com/tmtxt/conkerorrc>  
Follow me and we can exchange the experience.
