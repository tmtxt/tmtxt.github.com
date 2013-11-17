---
layout: post
title: "Emacs Spelling Checking on MacOS"
description: ""
category: Emacs
thumbnail: 
showtn: no
tags: [emacs, macos]
---
{% include JB/setup %}

Emacs is my favorite text editor and I use it as the main one for composing my
blog. However, sometimes when I'm typing too fast and I'm lazy to proofread my
article to some minor spelling error. Now I find a built-in solution in Emacs to
help me automatically find any spelling errors in my article.

Emacs is shipped with flyspell-mode and ispell, which use a command line program
for check the spelling. In most GNU/Linux distros, these programs are
pre-installed in the OS so you don't need to do anything in order to use
flyspell-mode, just type **M-x** and flyspell-mode.

However, MacOS is not shipped with any spell checker program so you will receive
the error `Searching for program: No such file or directory` when you try to
activate flyspell-mode in Emacs. We need to configure a bit for Emacs on MacOS
to run properly.

First, you need to install aspell, a spell checker program from macports. If
you're not familiar with macports, have a look at this post
[MacPorts - The MacOS package manager]({% post_url 2013-01-01-macports-the-macos-package-manager%}).
Install aspell from macports using this command

<!-- more -->

{% highlight console %}
$ sudo port install aspell
{% endhighlight %}

Next, you need to install the English dictionary for aspell. There are also
several available languages [here](http://aspell.net/man-html/Supported.html),
just replace **en** with the language code that you want to install

{% highlight console %}
$ sudo port install aspell-dict-en
{% endhighlight %}

Finally, add this to your .emacs to specify the path to aspell

{% highlight cl %}
(setq ispell-program-name "/opt/local/bin/aspell")
{% endhighlight %}

Now, when you want to use it, simply type **M-x** **flyspell-mode** or
**ispell**. You can type **flyspell-buffer** to re-check the whole buffer.
