---
layout: post
title: "Dired as Default File Manager - Dired Details"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, dired, dired-details, file manager]
---
{% include JB/setup %}

> This post is the second part of the series
> [Dired as Default File Manager](/2013/04/24/dired-as-default-file-manager-1-introduction/).

This part requires 2 more packages: dired-details and dired-details+. You
can easily install both of them through package.el (see this post
[Emacs Package Manager](/2013/01/07/emacs-package-manager/)).

To activate it, add this to your .emacs

<!-- more -->

{% highlight cl %}
(require 'dired-details+)
{% endhighlight %}

This picture illustrates how dired-details looks like. You can use **(** or
**)** to toggle hide (like the left window) or show (like the right window) details.

![Dired mode in my Emacs](/files/2013-04-06-dired-mode-as-default-file-manager/dired.png)  

Always show sym link targets

{% highlight cl %}
(setq dired-details-hide-link-targets nil)
{% endhighlight %}

Omit umimportant files (C-o to toggle)

{% highlight cl %}
(setq-default dired-omit-mode t
				dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
;; toggle omit mode C-o
(define-key dired-mode-map (kbd "C-o") 'dired-omit-mode)
{% endhighlight %}

**Previous part**:
[Dired as Default File Manager - Basic Tips](/2013/04/24/dired-as-default-file-manager-2-basic-tips/)  
**Next part**: [Dired as Default File Manager - More Advanced Tips](/2013/04/24/dired-as-default-file-manager-4-more-advanced-tips/)

