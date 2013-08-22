---
layout: post
showtn: yes
title: "Emacs - Setting up Perfect C/C++ programming environment - Part 2"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, programming environment]
---
{% include JB/setup %}

This is a continue to the first part in this post
[Emacs - Setting up perfect environment for C/C++ Programming - Part 1](/2013/03/10/emacs-setting-up-perfect-environment-for-cc-programming/).
This time, I'll demonstrate some more tips to help make your Emacs a powerful
IDE for C/C++ programming

# Syntax Checking on the fly

Flymake is a package which comes with Emacs 23 and above to help Emacs perform
syntax checking as you type. It uses an external program (usually the compiler)
to determine the errors.

Since it's a part of Emacs 23, you don't have to install in order to use it,
just add this to your .emacs

{% highlight cl %}
(require 'flymake)
{% endhighlight %}

<!-- more -->

Flymake requires a
makefile with the file name **Makefile** (capital M) to use with it. Also, your
makefile must contain the check-syntax target. Add this to your makefile (use
**tab** before gcc, not spaces)

{% highlight sh %}
check-syntax:
     gcc -o nul -S ${CHK_SOURCES}
{% endhighlight %}

Now to use it, simply open a C source code file and M-x **flymake** to activate
it. To avoid having manually activating flymake, it is configuarable to enable
flymake upon opening any file for which syntax checking is available.

{% highlight cl %}
(add-hook 'find-file-hook 'flymake-find-file-hook)
{% endhighlight %}

You may also want to config key bindings for
**flymake-display-err-menu-for-current-line**, **flymake-goto-next-error**,
**flymake-goto-previous-error** for quick navigation between errors.

# To be continue...
