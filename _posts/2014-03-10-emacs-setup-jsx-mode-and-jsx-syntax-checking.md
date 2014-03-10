---
layout: post
title: "Emacs - Setup JSX mode and JSX Syntax checking"
description: ""
category: emacs
tags: [emacs, jsx, jsxhint]
---
{% include JB/setup %}

If you are working with Javascript, especially ReactJS, you definitely known
about JSX, the XML syntax inside of Javascript. This post will demonstrate how
to setup Emacs for working with JSX files. First, you need to install `jsx-mode`
either by manually cloning and requiring the repo at
[https://github.com/jsx/jsx-mode.el](https://github.com/jsx/jsx-mode.el) or by
using [Emacs Packages Manager]({%post_url 2013-01-07-emacs-package-manager%}).
Add this to your .emacs to enable jsx-mode when you visit any .jsx file

{% highlight cl %}
(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
{% endhighlight %}

In `jsx-mode`, the following keys are bound by default.

* C-c C-c     comment-region (Comment or uncomment each line in the region)
* C-c c       jsx-compile-file (Compile the current buffer)
* C-c C       jsx-compile-file-async (Compile the current buffer asynchronously)
* C-c C-r     jsx-run-buffer (Run the current buffer)

<!-- more -->

Next, we need a tool for syntax checking. For Javascript, we have `jslint` (or
`jshint`), which I have previously written another post about how to set it up
in Emacs here
[Emacs - Setup JSHint for on-the-fly (potential) errors checking]({%post_url 2014-02-21-emacs-setup-jshint-for-on-the-fly-petential-error-checking%}).
For JSX, there is a similar tool called `jsxhint`. Of course, you need to
install it before you can use

{% highlight console %}
$ npm install -g jsxhint
{% endhighlight %}

You can test whether the tool works correctly by trying to check the syntax of a
JSX file

{% highlight console %}
$ jsxhint example.jsx
{% endhighlight %}

Now, to integrate it into Emacs, you need to install `flycheck` (a modern
version of `flymake`) through Emacs Packages Manager, too. The following piece
of code helps you define a new jsxhint checker in `flycheck` and activate it
automatically whenever you open a jsx file

{% highlight cl %}
(require 'flycheck)
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (jsx-mode))
(add-hook 'jsx-mode-hook (lambda ()
                          (flycheck-select-checker 'jsxhint-checker)
                          (flycheck-mode)))
{% endhighlight %}

**Note**: you need to make sure that `jsxhint` is located inside your Emacs'
path (or install `exec-path-from-shell` to import your shell's PATH
automatically).

![Alt Text](/files/2014-03-10-emacs-setup-jsx-mode-and-jsx-syntax-checking/flycheck.png)

