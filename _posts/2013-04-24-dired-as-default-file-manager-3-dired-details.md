---
layout: post
showtn: yes
title: "Dired as Default File Manager - Show the interesting and Hide the unnecessary information"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, dired, dired-details, file manager]
---
{% include JB/setup %}

> This post is the second part of the series
> [Dired as Default File Manager](/2013/04/24/dired-as-default-file-manager-1-introduction/).

# dired-details & dired-details+

This section requires 2 more packages: dired-details and dired-details+. You
can easily install both of them through package.el (see this post
[Emacs Package Manager](/2013/01/07/emacs-package-manager/)).

To activate it, add this to your .emacs

{% highlight cl %}
(require 'dired-details+)
{% endhighlight %}

This picture illustrates how dired-details looks like. You can use `(` or
`)` to toggle hide (like the left window) or show (like the right window) details.

<!-- more -->

![Dired mode in my Emacs](/files/2013-04-06-dired-mode-as-default-file-manager/dired.png)  

Also in dired-details, to show sym link targets, add this to our .emacs

{% highlight cl %}
(setq dired-details-hide-link-targets nil)
{% endhighlight %}

# Dired Omit Mode

Dired Omit Mode is a feature in **dired-x.el**, which come built-in with Emacs.

> Omitting a file means removing it from the directory listing. Omitting is
> useful for keeping Dired buffers free of “uninteresting” files (for instance,
> auto-save, auxiliary, backup, and revision control files) so that the user can
> concentrate on the interesting files.

To activate it, add this to your .emacs

{% highlight cl %}
(setq-default dired-omit-mode t)
{% endhighlight %}

To toggle the mode, bind it to a keystroke that you like

{% highlight cl %}
(define-key dired-mode-map (kbd "C-o") 'dired-omit-mode)
{% endhighlight %}

`dired-omit-files` contains the regex of the files to hide in Dired Mode. For
example, if you want to hide the files that begin with . and #, set that variable
like this

{% highlight cl %}
(setq-default dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
{% endhighlight %}

The variable `dired-omit-extensions` holds the list of all file extensions to
hide in Dired Omit Mode. You can modify the list to add more file extensions to
hide or remove the ones that you would like to show.

{% highlight cl %}
(add-to-list 'dired-omit-extension ".example")
(delete 'dired-omit-extension ".example")
{% endhighlight %}

**Note**: If you are using Dired Omit Mode with dired+, remember to put the
config of Dired Omit Mode before loading (`require`) dired+ since some feature
of dired+ use the config from Dired Omit Mode (for example for displaying the
file names).

**Previous part**:
[Dired as Default File Manager - Basic Tips](/2013/04/24/dired-as-default-file-manager-2-basic-tips/)  
**Next part**: [Dired as Default File Manager - More Advanced Tips](/2013/04/24/dired-as-default-file-manager-4-more-advanced-tips/)
