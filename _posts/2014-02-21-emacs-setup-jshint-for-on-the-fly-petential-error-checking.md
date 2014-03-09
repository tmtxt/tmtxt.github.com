---
layout: post
title: "Emacs - Setup JSHint for on-the-fly (potential) errors checking"
description: ""
category: emacs
tags: [emacs, jshint, javascript]
---
{% include JB/setup %}

# JSHint command line tool

**JSHint** is a tool that helps to detect errors and potential problems in your
JavaScript code. You can try the online demo version at JSHint home page at
[http://www.jshint.com/](http://www.jshint.com/). This post demonstrates how to
setup jshint and integrate it into Emacs for javascript development on Nodejs.

Firstly, make sure you have installed **nodejs** and its package manager (npm).
Next, install jshint globally using npm with this command

{% highlight console %}
$ npm install -g jshint
{% endhighlight %}

You can try linting your js file with jshint

{% highlight console %}
$ jshint myfile.js
{% endhighlight %}

# JSHint with Flycheck

Before that, I used `jshint` with `flymake`. However, now I found that
`flycheck` (an improvement of `flymake`) has built-in support for jshint so you
don't need to install **jshint-mode** anymore. Just install `flycheck` using
package.el
([Emacs Packages Manager]({%post_url 2013-01-07-emacs-package-manager%})). Add
this to your .emacs to activate **flycheck** when you visit any .js file

<!-- more -->

{% highlight cl %}
(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
{% endhighlight %}

# JSHint with Flymake

This is not a recommended method since it uses the old flymake syntax checking.
To install jshint with flymake, clone the **jshint-mode** for Emacs to your
local computer

{% highlight console %}
$ git clone git://github.com/daleharvey/jshint-mode.git
{% endhighlight %}

Add this to your .emacs to load jshint-mode into Emacs

{% highlight cl %}
(add-to-list 'load-path "~/path/to/jshint-mode")
(require 'flymake-jshint)
(add-hook 'js-mode-hook
     (lambda () (flymake-mode t)))
{% endhighlight %}

In case your Emacs cannot find the node executable, you can install
**exec-path-from-shell** using **package.el** for Emacs to load the PATH from your shell
config.

Now everything you need to do is just to open a javascript file and
flymake-jshint is automatically activated for you.
