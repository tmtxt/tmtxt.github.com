---
layout: post
title: "Emacs - Javascript Completion and Refactoring"
description: ""
categories: [emacs, javascript]
tags: []
---


# 1. js2-refactor

This is one of the simplest refactoring library for Emacs. It is written
entirely in Emacs and does not require any external program to work with. It is
also designed for
working with [js2-mode](https://github.com/mooz/js2-mode), one of the best
Javascript IDE for Emacs. If you haven't known about **js2-mode** yet, take a
look at this post
[Set up Javascript development environment in Emacs]({%post_url 2014-02-23-set-up-javascript-development-environment-in-emacs%}).
Some noteworthy features of **js-refactor** are
expand/contract functions, objects or arrays, lexical scope variable
renaming,... You can easily install it using
[package.el]({%post_url 2013-01-07-emacs-package-manager%}).

{% highlight cl %}
M-x package-install js2-refactor
{% endhighlight %}

All the available functions of js2-refactor is available on github. Spend about 5
minutes to familiarize yourself with it
[https://github.com/magnars/js2-refactor.el](https://github.com/magnars/js2-refactor.el).

# 2. Tern.js - Intelligent Javascript tooling

## 2.1 Tern.js basic

Tern is a stand-alone code-analysis engine for JavaScript. You can use
**Tern.js** for these tasks

* Auto completion on variables and properties
* Function argument hints
* Querying the type of an expression
* Finding the definition of something
* Automatic refactoring

There is an online demo that you can try at this
[link](http://ternjs.net/doc/demo.html).

<!-- more -->

To use it, you need to install Tern binary executable using **npm**

{% highlight console %}
$ npm install -g tern
{% endhighlight %}

The next thing you need to do is to install Tern packages for Emacs. The auto
completion feature uses auto-complete so make sure you are familiar with
auto-complete.

{% highlight cl %}
M-x package-install tern
M-x package-install tern-auto-complete
{% endhighlight %}

Add this to your .emacs to initialize tern and tern-auto-complete

{% highlight scm %}
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
{% endhighlight %}

For the auto completion to work properly in your Node.js project, you need to do
some config. Open your project directory and create a file named `.tern-project`
with the content like this

{% highlight json %}
{
  "plugins": {
    "node": {
    }
  }
}
{% endhighlight %}

For more details on the `.tern-project` file as web as how to include some
plugins like AngularJS or RequireJS, visit the
[Tern Docs](http://ternjs.net/doc/manual.html#configuration). In my case, just
the above file is enough.

By default, Tern binds `.` to `tern-ac-dot-complete` if `tern-ac-on-dot` is
non-nil. Typing `.` after a variable will cause Tern to read that object's
properties and show the suggestion. This can be a bit slow when you first open
the file since it have to start the Tern server but the next time when you
invoke `tern-ac-dot-complete`, it will be much faster. A file named `.tern-port`
will also be created automatically in your project's root folder.

There are some other important keys that are bound already by default

* `M-.` Jump to the definition of the thing under the cursor.
* `M-,` Brings you back to last place you were when you pressed M-..
* `C-c C-r` Rename the variable under the cursor.
* `C-c C-c` Find the type of the thing under the cursor.
* `C-c C-d` Find docs of the thing under the cursor. Press again to open the associated URL (if any).

![Alt Text](/files/2014-04-20-emacs-javascript-completion-and-refactoring/tern.gif)

## 2.2 Fix error when Tern does not auto refresh

Sometimes when you have just added `.tern-project` file or edit the file but
Tern does not auto reload, you need to manually kill Tern server. This little
piece of code does the trick

{% highlight cl %}
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))
{% endhighlight %}

When you encounter that problem, just activate the command `delete-tern-process`
via `M-x` and then continue using as normal. Tern server will be launched
automatically.

# 3. Emacs Tags with Exuberant Ctags

If you are happy with **Tern.js**, stick with it, you probably don't need
**Ctags**.

[Ctags](http://ctags.sourceforge.net/) is a program for generating an index file
of language objects found in source files. You can then use that file for
quickly searching and locating those language objects using a text editor.
**Ctags** supports more than 40 languages include C, C++, C#, Javascript,... You
can visit this [link](http://ctags.sourceforge.net/languages.html) for the full
list of supported programming languages. Every time you changes your source
file, you need to regenerate the tag file. **Ctags** can be installed easily using
your package manager, for example with Macports

{% highlight console %}
$ port install ctags
{% endhighlight %}

To build the tag for your project, activate ctags with `-e` and `-R` flags

{% highlight console %}
$ cd /path/to/project
$ ctags -e -R --exclude=node_modules --exclude=public
{% endhighlight %}

A file named **TAGS** will be created inside the project root folder containing
all the information about the project source code. Now open any .js file in your
project, put the cursor on any function or variable name and activate
`etags-select-goto-tag` to quickly jump to that function definition. However,
the default Emacs' etag feature is not quite good. You should use etags provided
by [helm](https://github.com/emacs-helm/helm) (can be installed via package.el).
Instead of activating `etags-select-goto-tag`, try `helm-etags-select`. If the
project is big, It take some time to load tag file. But when it is done, the
next search will be very fast.

![Alt Text](/files/2014-04-20-emacs-javascript-completion-and-refactoring/etags.gif)
