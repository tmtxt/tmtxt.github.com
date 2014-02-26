---
layout: post
showtn: yes
title: "Emacs - C/C++ Autocomplete with Clang"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [clang, emacs, autocomplete]
---
{% include JB/setup %}

# Introduction

This article is to help you config Emacs to use Clang autocomplete source for
C/C++ and even Objective C programming.

If you haven't known what is Clang yet, have a look at this article
[Clang on Wikipedia](http://en.wikipedia.org/wiki/Clang)

# Requirements & Installation

First, you have to install AutoComplete for Emacs. You can install it using
[Emacs Package Manager](/2013/01/07/emacs-package-manager/). The next step is to
install Yasnippet. This is not compulsory but is encouraged for better
experiments. You can also install it using Emacs Package Manager. To see how to
get Autocomplete and Yasnippet to work together, see this article
[Config Yasnippet and Autocomplete on Emacs](/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/).

<!-- more -->

Before installing this package, you should also check whether your system has
installed "clang" or not via this command `clang --version` in terminal. If you get
nothing from terminal, which means you haven't installed "clang" yet. If you are
on Ubuntu, you can try those 2 commands `sudo apt-get update`,
`sudo apt-get install clang` and check it again. Until clang works fine, you can
continue the
following instructions.

If your computer has satisfied those requirements, now you can start installing
Clang autocomplete. In fact, it's a source for Autocomplete plugin, that's why
we need Autocomplete installed.

You need to clone the
[auto-complete-clang source](https://github.com/brianjcj/auto-complete-clang)
from github. I advise you to clone it into your .emacs.d directory. After that,
add this to your .emacs or init.el (if you use the modern style of .emacs) to
load it to emacs' load-path and activate auto-complete-clang.

{% highlight cl %}
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
(require 'auto-complete-clang)
{% endhighlight %}

Finally, bind it to a key binding for using. I bind it to **C-\`** so that it do
not conflict with autocomplete or yasnippet

{% highlight cl %}
(global-set-key (kbd "C-`") 'ac-complete-clang)
{% endhighlight %}

# Screenshot

Here is the screenshot from my emacs

![auto-complete-clang](/files/2013-03-06-emacs-ccpp-autocomplete-with-clang/clang.png)
