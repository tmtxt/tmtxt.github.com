---
layout: post
showtn: yes
title: "Emacs - Sort Directories first in Dired"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, dired]
---
{% include JB/setup %}

> This post is now moved to new address at [Dired as Default File Manager - Customize ls command](/2013/04/25/dired-as-default-file-manager-5-customize-ls-command/)

<!-- more -->

# Problem with Emacs Dired mode

Emacs now become my daily used application because of it convenience. I even use
it as the default file manager application with its built-in Dired mode.
However, dired mode puts everything, including files and directories, together,
which makes things harder for me to read and organize.

# Solution

After days googling, I found that dired displays the content of the directory
based on the result returned from **ls** command (of course, I'm talking about
Unix-based system, not Windows). We can change the way it displays by adding
some more arguments for the **ls** command.

# Preparation ls program

If you are using a GNU Linux based OS, everything is just simple. The GNU
version of **ls** has the option to sort directories first.

Unfortanately, the MacOS version doesn't. But don't worry, you can install the
GNU one from Macports. If you haven't installed MacPorts yet, take a look at this
[article](/2013/01/01/macports-the-macos-package-manager/). After finishing
installing MacPorts, open up terminal and install **coreutils** and
**findutils** by this command

{% highlight sh %}
$ sudo port install coreutils findutils
{% endhighlight %}

If you want to use those GNU version commands, eg **ls**, **cp**, **chmod**,
etc, in terminal, add this to your .bash_profile or .zshrc

{% highlight sh %}
$ export PATH=$PATH:/opt/local/libexec/gnubin
{% endhighlight %}

After that, restart terminal and type this command to test

{% highlight sh %}
$ where ls
{% endhighlight %}

If you see this path **/opt/local/libexec/gnubin/ls** in the output result,
congratulation, you have installed it successfully.

If you want to test whether it can sort files correctly (both Mac and Linux),
execute this command

{% highlight sh %}
$ ls --group-directories-first -alh
{% endhighlight %}

If it runs with no error and the result is correct (directories on top), you can
continue to the next part.

# Config Emacs Dired

Before proceeding to this part, if you're on MacOS, you will need one extra step
to get Dired to use the new **ls** program that you've just installed,
instead of the default one on MacOS. Add this to your .emacs

{% highlight cl %}
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "/opt/local/libexec/gnubin/ls")
{% endhighlight %}

The last step is to config Dired mode. You need to tell Dired to add more
arguments to the **ls** call so that it can display correctly. The library
called **dired-sort-map**, which you can find
[here](http://emacswiki.org/emacs/dired-sort-map.el), can help you achieve that
task. Download the **dired-sort-map.el** file from the link above, put it in
your .emacs.d folder or somewhere that Emacs can find it in the load path.
Finally, add those lines to your .emacs

{% highlight cl %}
(require 'dired-sort-map)
(setq dired-listing-switches "--group-directories-first -alh")
{% endhighlight %}

Restart Emacs or simply just evaluate that code and open a new Dired buffer to
see the changes.
