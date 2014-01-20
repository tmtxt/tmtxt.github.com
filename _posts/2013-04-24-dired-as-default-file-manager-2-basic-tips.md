---
layout: post
showtn: yes
title: "Dired as Default File Manager - Basic Tips"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, dired, file manager]
---
{% include JB/setup %}

> This post is the first part of the series
> [Dired as Default File Manager](/2013/04/24/dired-as-default-file-manager-1-introduction/).

Before reading this, you should have a look at this page
[Dired - GNU Emacs Manual](http://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html)
to know some basic commands to use Dired properly.

# Always Recursion

* Always recursively delete directory

{% highlight cl %}
(setq dired-recursive-deletes 'always)
{% endhighlight %}

* Always recursively copy directory

{% highlight cl %}
(setq dired-recursive-copies 'always)
{% endhighlight %}

# Auto guess target

Set this variable to non-nil, Dired will try to guess a default target
directory. This means: if there is a dired buffer displayed in the next window,
use its current subdir, instead of the current subdir of this dired buffer. The
target is used in the prompt for file copy, rename etc.

<!-- more -->

{% highlight cl %}
(setq dired-dwim-target t)
{% endhighlight %}

# Delete by moving to Trash.
Replace **~/.Trash/emacs** with the path to your trash folder.

{% highlight cl %}
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")
{% endhighlight %}

# Dired+

Dired+ is a package that extends functionalities provided by standard GNU Emacs
libraries dired.el, dired-aux.el, and dired-x.el. You can install through
package.el (see this post
[Emacs Package Manager](/2013/01/07/emacs-package-manager/)). Some of my tips in
this series use Dired+.

**Next part**:
[Dired as Default File Manager - Show the interesting and Hide the unnecessary information](/2013/04/24/dired-as-default-file-manager-3-dired-details/)
