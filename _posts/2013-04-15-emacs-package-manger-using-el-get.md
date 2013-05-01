---
layout: post
showtn: yes
title: "Emacs Package Manger using el-get"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, package manger, el-get]
---
{% include JB/setup %}

In one of my previous post
([Emacs Package Manager](/2013/01/07/emacs-package-manager/)), I have
demonstrated how to manage packages within Emacs as well as auto install missing
packages using ELPA. Today, I've discovered another package manager for Emacs,
which is very powerful and especially suitable for complex packages called
[el-get](http://www.emacswiki.org/emacs/el-get). It can help us simplify the
installation process by automatically fetch and install the package that you
want as well as all its dependencies.

<!-- more -->

# Installation

el-get needs to be installed in **~/.emacs.d/el-get/el-get**. Create a directory
named el-get inside your .emacs.d folder. Open terminal, change to that
directory and clone the el-get repo.

{% highlight console %}
$ cd ~/.emacs.d/
$ mkdir el-get
$ cd el-get
$ git clone git@github.com:dimitri/el-get.git
{% endhighlight %}

Add this to your .emacs or init.el file

{% highlight cl %}
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
{% endhighlight %}

# Auto install missing packages

I have written this small piece of code for el-get to install those packages I
need automatically. Everytime Emacs starts, it will check
if all those packages I need are not installed yet and then let el-get install
them in the background.
I used to backup my emacs init file into github. This piece of code is extremely
useful because
I can ignore all the compiled code directories and commit only my init files.
When I have to reinstall my computer OS or change to another one, what I have to
do is just to clone my emacs init files and then it will take care all the
installation process as well as load-path manager for me.

{% highlight cl %}
;;; auto install missing packages
;;; everytime emacs starts, it will check for those packages, if they are not
;;; installed, auto install them
(defvar tmtxt/el-get-packages
  '(package1 package2 package3 package4))
(dolist (p tmtxt/el-get-packages)
  (when (not (el-get-package-exists-p p))
	(el-get-install p)))
;;; sync packages
(el-get 'sync)
{% endhighlight %}

Replace package1 package2 package3 package4 with the name of the package you
want to install.
