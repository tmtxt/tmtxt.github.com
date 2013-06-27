---
layout: post
showtn: yes
title: "Emacs - Setting up perfect environment for C/C++ Programming - Part 1"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, programming environment]
---
{% include JB/setup %}

Emacs is an ideal text editor for programmers, especially C/C++ programmers. I
have been using Emacs since I began learning C while my classmates used vim,
which was taught by the lecturer. Now I haven't found any better solution for me
in C/C++ programming.

<!-- more -->

# Basic configuartion

By default, Emacs automatically enables major C/C++ mode whenever you open a
file with extension .h, .c or .cpp. In case your Emacs does not behave that way,
add this to your .emacs

{% highlight cl %}
(require 'cc-mode)
{% endhighlight %}

You will also want Emacs to indent the code for you so that it will be much
easier for you to follow. To do that, add this to your .emacs

{% highlight cl %}
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
{% endhighlight %}

Another thing to consider is the brackets. Often when I program, I want the
computer to add the closing bracket for me so that I can avoid those silly
errors. **Autopair** is a plugin package for emacs that can help us achieve that
task. You can install it through package.el or cloning it from the source. After
finishing installation, make sure that it's in your load-path and then add this
to your .emacs

{% highlight cl %}
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)
{% endhighlight %}

# ECB - Transform Emacs to a real IDE

Have a look at this post
[Install and Config ECB on Emacs]({% post_url 2013-03-10-ecb-emacs-code-browser %})
and the screenshot below to see how powerful and useful ECB is. You can also
find the installation and configuration guide in that post above.

![ECB Screenshot](/files/2013-03-10-ecb---emacs-code-browser/ecb.png )

# Auto Completion and Code Snippets

Emacs cannot compete with the other IDE if it doesn't have those auto completion
and snippet inserting function. They improve the programmers' effeciency very
much. Read this article
[Config Yasnippet and Autocomplete on Emacs](/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/)
to know how to config autocomplete and yasnippet so that they can work together.

Another package that you must have is auto-complete-clang, a clang source for
autocomplete. Make sure to install autocomplete and yasnipper before installing
auto-complete-clang (you can easily install it using packages.el). When you
finish the installation, bind it with a key sequence for quick access (remember
to check whether it is in the load-path).

{% highlight cl %}
(require 'auto-complete-clang)
(define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-clang)
;; replace C-S-<return> with a key binding that you want
{% endhighlight %}

The last package that I'd like to recomend you is the **member-function**
package. It is extemely useful for those who program C++. This package helps you
expand the function headers defined in .h file to a .cpp file. Install it using
package.el and then add this to your init file

{% highlight cl %}
(require 'member-function)
(setq mf--source-file-extension "cpp")
{% endhighlight %}

**Continue to Part 2 here**:
[Emacs - Setting up Perfect C/C++ programming environment - Part 2](/2013/04/12/emacs-setting-up-perfect-cc-programming-environment/)
