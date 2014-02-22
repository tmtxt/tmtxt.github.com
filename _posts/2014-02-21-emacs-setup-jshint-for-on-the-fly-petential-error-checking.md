---
layout: post
title: "Emacs - Setup JSHint for on-the-fly (potential) errors checking"
description: ""
category: emacs
tags: [emacs, jshint, javascript]
---
{% include JB/setup %}

**JSHint** is a tool that helps to detect errors and potential problems in your
JavaScript code. You can try the online demo version at JSHint home page at
[http://www.jshint.com/](http://www.jshint.com/). This post demonstrates how to
setup jshint and integrate it into Emacs for javascript development on Nodejs.

Firstly, make sure you have installed **nodejs** and its package manager (npm).
Next, install jshint globally using npm with this command

{% highlight console %}
$ npm install -g jshint
{% endhighlight %}

Next, clone the **jshint-mode** for Emacs to your local computer

{% highlight console %}
$ git clone git://github.com/daleharvey/jshint-mode.git
{% endhighlight %}

<!-- more -->

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

Now open a javascript file inside a nodejs project, `M-x` and then
`flymake-mode` to activate jshint.
