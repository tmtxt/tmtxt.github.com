---
layout: post
showtn: yes
title: "Dired as Default File Manager - Customize ls command"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, dired, file manager, ls]
---
{% include JB/setup %}

> This post is the fourth part of the series
> [Dired as Default File Manager](/2013/04/24/dired-as-default-file-manager-1-introduction/).

# How Dired works

Dired displays the content of the directory
based on the result returned from **ls** command (of course, I'm talking about
Unix-based system, not Windows). We can customize the way it displays by adding
some more arguments for the **ls** command.

# Prepare ls program

If you are using a GNU Linux based OS, everything is just simple. The GNU
version of **ls** has many options for you to play with and you can skip to the
next part.

<!-- more -->

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
$ export PATH=/opt/local/libexec/gnubin:$PATH:
{% endhighlight %}

After that, restart terminal and type this command to test

{% highlight sh %}
$ where ls
{% endhighlight %}

If you see this path **/opt/local/libexec/gnubin/ls** in the output result,
congratulation, you have installed it successfully.

Before you proceed to the next part, you need to config Dired to use your newly
installed **ls** program, not the default one in MacOS. Add this to your .emacs

{% highlight cl %}
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "/opt/local/libexec/gnubin/ls")
{% endhighlight %}

# Customize Dired mode with ls command

The last step is to config Dired mode. You need to tell Dired to add more
arguments to the **ls** call so that it can display correctly. The library
called **dired-sort-map**, which you can find
[here](http://emacswiki.org/emacs/dired-sort-map.el), call help you achieve that
task. Download the **dired-sort-map.el** file from the link above, put it in
your .emacs.d folder or somewhere that Emacs can find it in the load path and
add this to your .emacs

{% highlight cl %}
(require 'dired-sort-map)
{% endhighlight %}

The variable **dired-listing-switches** specifies the extra argument that you
want to pass to **ls** command. For example, calling **ls
--group-directories-first** will result in ls sort the directories first in the
output. To let Emacs pass that argument to ls, use this code

{% highlight cl %}
(setq dired-listing-switches "--group-directories-first")
{% endhighlight %}

You can add more arguments that you like, in that case the code will look like
this

{% highlight cl %}
(setq dired-listing-switches "--group-directories-first -alh")
{% endhighlight %}

That means calling **ls** with the arguments --group-directories-first, -a, -l,
-h arguments.

Some of the arguments that you might be interested in:

* -a: list all files
* -h, --human-readable: with -l, print sizes in human readable format (e.g., 1K 234M 2G)
* -l: use a long listing format
* --group-directories-first: group directories before files
* -X: sort alphabetically by entry extension
* -U: do not sort; list entries in directory order
* -t: sort by modification time, newest first
* -S: sort by file size

For more arguments, simply open terminal and type

{% highlight console %}
$ ls --help
{% endhighlight %}

**Previous part**:
[Dired as Default File Manager - More Advanced Tips](/2013/04/24/dired-as-default-file-manager-4-more-advanced-tips/)  
**Next part**: 
[Dired as Default File Manager - Customize Faces](/2013/04/25/dired-as-default-file-manager-6-customize-faces/)
