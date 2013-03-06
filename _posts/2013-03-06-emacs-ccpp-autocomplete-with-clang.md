---
layout: post
title: "Emacs - C/C++ Autocomplete with Clang"
description: ""
category: 
thumbnail: 
tags: [clang, emacs, tutorial, autocomplete]
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
get Autocomplete and Yasnippet work together, see this article
[Config Yasnippet and Autocomplete on Emacs](/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/).

If you have all those requirements, now you can start installing Clang
autocomplete. In fact, it's a source for Autocomplete plugin, that's why we need
Autocomplete installed.

You need to clone the
[auto-complete-clang source](https://github.com/brianjcj/auto-complete-clang)
from github. I advise cloning it into your .emacs.d directory. After that, add
this to your .emacs or init.el (if you use the modern style of .emacs) to load
it to emacs' load-path and activate auto-complete-clang.

{% highlight cl %}
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
(require 'auto-complete-clang)
{% endhighlight %}

Finally, bind it to a key binding for using. I bind it to <span>C-`</span> so that it do
not conflict with autocomplete or yasnippet

{% highlight cl %}
(global-set-key (kbd "C-`") 'ac-complete-clang)
{% endhighlight %}

# Screenshot

Here is the screenshot from my emacs

![auto-complete-clang](/files/2013-03-06-emacs-ccpp-autocomplete-with-clang/clang.png)
