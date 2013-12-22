---
layout: post
title: "Emacs - Search for text occurences in multiple files with grep"
description: ""
category: emacs
tags: [emacs, grep]
---
{% include JB/setup %}

# Finding text in Emacs

When working with a project with large number of files, there will be the case
that we cannot remember where a piece of text come from or we want to search
where one function, variable is used. There are many ways to achieve this and
using the `grep` command in combination with Emacs is a very efficient method.

# 1. find-grep, grep-find and rgrep

The command `find-grep` (also aliased as `grep-find`), which you can activate
via **M-x**, helps you run **grep** via **find** and then display all the
results in a `*grep*` buffer. To use it, simply open the directory you need
to find in dired mode and call `find-grep`. The command is already filled for
you. All you need is to type the string pattern and press **RET** for grep to
start working. All the result will be display in a new window, just select the
item you want and hit **RET** to visit that file.

`rgrep` is another impressive command which behave similar to `find-grep` but it's
more interative and has more configuration to play with. Once activated, `rgrep`
will ask you for the search string regex (default is the word at point), file
types to search for (all or just one specific file type) and the base directory
(default is the current working directory). Also, you can config `rgrep` to
skip specified files or folders by add them to the two variables
**grep-find-ignored-files** and **grep-find-ignored-directories**.

<!-- more -->

{% highlight cl %}
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-directories)
     (add-to-list 'grep-find-ignored-directories "*.bin")))
{% endhighlight %}

<a href="/files/2013-12-22-emacs-search-for-text-occurences-with-grep/rgrep.png" target="_blank"><img src="/files/2013-12-22-emacs-search-for-text-occurences-with-grep/rgrep.png" style="width:600px;display:block;margin-left:auto;margin-right:auto;" /></a>

# 2. grep with helm

`helm-do-grep` is another interesting command comes with **helm**. You can
install **helm** using elpa. Also, you can take a look at
[helm wiki](https://github.com/emacs-helm/helm/wiki#wiki-grep) to investigate
more about **helm**. Similar to the above `rgrep` command, `helm-do-grep` will
also interactively ask you for some information about the string pattern, the
base path as well as the file types to search for. If you call `helm-do-grep`
normally, it will search only for the current directory. A prefix **C-u** arg
will launch recursive grep for all sub directories.

The most impressive feature of `helm-do-grep` is the live updating result as you
type. That means the grep process is started immediately while you are typing so
you will see the result being updated continuously.

<a href="/files/2013-12-22-emacs-search-for-text-occurences-with-grep/helm.png" target="_blank"><img src="/files/2013-12-22-emacs-search-for-text-occurences-with-grep/helm.png" style="width:600px;display:block;margin-left:auto;margin-right:auto;" /></a>

# Reference
* [GNU Emacs Manual](http://www.gnu.org/software/emacs/manual/html_node/emacs/Grep-Searching.html)
* [helm wiki](https://github.com/emacs-helm/helm/wiki#wiki-grep)
