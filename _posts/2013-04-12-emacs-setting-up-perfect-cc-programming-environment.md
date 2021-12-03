---
layout: post
showtn: yes
title: "Emacs - Setting up C/C++ programming environment - Part 2"
description: ""
category: emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, programming environment]
---


This is a continue to the first part in this post
[Emacs - Setting up environment for C/C++ Programming - Part 1](/2013/03/10/emacs-setting-up-perfect-environment-for-cc-programming/).
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

# Jump to Definition / Jump to Implementation

Thank [To Hoang Do](https://www.facebook.com/tu.h.do.16) for providing me the
solution (in the comments).

For jumping between definitions and implementations, you can use gtags, ctags or
cscope, [ggtags](https://github.com/leoliu/ggtags). Emacs has packages for
theses source navigation tool.

If you want to use for other languages, setup gtags with ctags backend follow
these instructions:
[https://github.com/leoliu/ggtags/wiki/Install-Global-with-support-for-exuberant-ctags](https://github.com/leoliu/ggtags/wiki/Install-Global-with-support-for-exuberant-ctags).

Or use ctags itself with default `find-tags` in Emacs. However, I recommend you
to use GNU Global with ggtags for C/C++/Assembly. Use ctags for everything else.

Whatever packages you are using, make sure you use Helm. Here is an example of
Helm, using CEDET and function-args packages

![CEDET Helm](/files/2013-04-12-emacs-setting-up-perfect-cc-programming-environment/get.gif)
