---
layout: post
title: "MacOS - Spotlight Search inside Emacs"
description: ""
category: emacs
tags: [macos, spotlight, locate, emacs, helm]
---
{% include JB/setup %}

MacOS comes with a powerful search tool, Spotlight, that helps us find any file
in our computer at a glance. It's would be great if we can integrate it with
Emacs and use it as a
replacement for the default Emacs' `find-file` command so that we can jump to any
file we want to edit without remembering the location. Luckily, there is an
useful program called `mdfind`, a command line interface for Spotlight. We will
use it in combination with the `locate` (or `helm-locate` if you're using **helm**) command.

# 1. Emacs built-in "locate" command

The step to use Spotlight with `locate` command is really simple. You just need
to set the variable `locate-command` to
**mdfind** to let Emacs use **mdfind** instead of **locate** for searching file.

{% highlight cl %}
(setq locate-command "mdfind")
{% endhighlight %}

When you want to use it, type in **M-x** **locate** and enter the keyword to
search. The result will be displayed in a new buffer. Simply press Return on the
file to open it for editing. If you follow the instruction in this post
[Dired as Default File Manager - More Advanced Tips]({% post_url 2013-04-24-dired-as-default-file-manager-4-more-advanced-tips %})
on how to open file using MacOS default program, you can use that
function/keystroke to open the file in search result with that default program.

This method is super fast since Spotlight has already indexed your Mac while you
use it. Usually, it takes me just 1-2 seconds to display all the result and it's
can be faster on a Mac running an SSD.

<!-- more -->

Demo Image:

![Alt Text](/files/2013-09-13-macos-spotlight-search-inside-emacs/locate.png)

# 2. Using "helm-locate" command

`helm-locate` is a similar command to the Emacs' built-in `locate` command.
However, `helm-locate` allow for live updating result as you type. To use it,
you need to install `helm` first. Activate it using `M-x`, `helm-locate` and
type in what you need to search for.

![Alt Text](/files/2013-09-13-macos-spotlight-search-inside-emacs/helm.png)
