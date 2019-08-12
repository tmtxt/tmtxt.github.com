---
layout: post
title: "Set up Javascript development environment in Emacs"
description: ""
categories: [emacs, javascript]
tags: [emacs, javascript]
---


The built-in js-mode in Emacs does not provide many
features for working with js framework beside js editing and syntax
highlighting. The tips in this post will help you transform your Emacs into a
powerful Javascript IDE.

# Associate files with js mode

By default, open a file with **.js** extension will automatically activate
js-mode. For some other file types, if you want to link them to js-mode, add
this to your .emacs

{% highlight cl %}
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
{% endhighlight %}

# js2-mode - A better js mode for Emacs

> js2-mode by SteveYegge is arguably the best JavaScript mode available for
> emacs. It has very accurate syntax highlighting, supports newer JavaScript
> extensions implemented in SpiderMonkey, and highlights syntax errors as you
> type.

The easiest way to install `js2-mode` is via package.el
([Emacs Package Manager]({%post_url 2013-01-07-emacs-package-manager%})). Also,
you need to install `ac-js2` for using js2-mode with auto-complete suggestion. Once
installed, add this to your .emacs to activate js2 and its auto-complete.

{% highlight cl %}
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
{% endhighlight %}

<!-- more -->

js2-mode comes with some useful utility functions for working with js files more
efficiently. For example, `ac-js2-jump-to-definition` quickly jumps to the
definition of one variable/function that is defined in the same file,
`js2-mark-defun` selects the current function,... You can use the command
`apropos-command` to list all js2 commands.

js2-mode provides 4 level of syntax highlighting. They are
* 0 or a negative value means none.
* 1 adds basic syntax highlighting.
* 2 adds highlighting of some Ecma built-in properties.
* 3 adds highlighting of many Ecma built-in functions.

To set the amount of syntax highlighting to perform, change the value of the
variable to the level that you want

{% highlight cl %}
(setq js2-highlight-level 3)
{% endhighlight %}

# Auto completion

**autocomplete** and **yasnippet** are two powerful tools for code suggestion and
completion in Emacs. **autocomplete** will scan for all words in the current
buffer as well as other opening buffers and in the history to complete the word
as you're typing. **yasnippet** provides the ability to quickly insert
repetitive snippets such as `for`, `while`, `if`,...

Take a look at this link to know how to install, config autocomplete and
yasnippet in Emacs
[Config Yasnippet and Autocomplete on Emacs]({%post_url 2013-01-06-config-yasnippet-and-autocomplete-on-emacs%})

For more complex suggestion like completion from other files, completion using
Nodejs modules, requirejs modules,... see the
[Javascript Refactoring](#javascript-refactoring) section below.

If you are working with Facebook React, you can install
[react-snippets](https://github.com/johnmastro/react-snippets.el)
from MELPA. It is a collection of common snippets for React.

# Using paredit with javascript

**Paredit** is a minor mode for keeping parenthese balanced. It is extremely
useful for working with lisp-based programming languages. Also, it is activated
automatically when you open any lisp-based languages file. Usually in other
languages, we have to work with parentheses, too. For example the **{** and
**}** for code block, **\[** and **\]** for array...

This post describes how you can activate paredit for non-lisp mode
[Emacs - Using paredit with non lisp mode]({%post_url 2014-02-22-emacs-using-paredit-with-non-lisp-mode%}).

You may also want to set `{` and `}` to the corresponding paredit functions

{% highlight cl %}
(define-key js-mode-map "{" 'paredit-open-curly)
(define-key js-mode-map "}" 'paredit-close-curly-and-newline)
{% endhighlight %}

# JSHint - Detect errors and potential problems in your js code

**JSHint** is a community-driven tool to detect errors and potential problems in
JavaScript code and to enforce your team's coding conventions. It is very
flexible so you can easily adjust it to your particular coding guidelines and
the environment you expect your code to execute in.

You can read more information at **js-hint** [homepage](http://www.jshint.com/).
I have another post about setting up and integrating it into Emacs here
[Emacs - Setup JSHint for on-the-fly (potential) errors checking]({%post_url 2014-02-21-emacs-setup-jshint-for-on-the-fly-petential-error-checking%}).

# Emacs for JSX coding

JSX is also a perfect development environment for JSX code. The instruction are
presented all in this post
[Emacs - Setup JSX mode, JSX Syntax checking and Suggestion]({%post_url 2014-03-10-emacs-setup-jsx-mode-and-jsx-syntax-checking%})

# Mozrepl - Program Mozilla-based applications from the inside

Mozrepl is an extension for Mozilla-based applications that helps you to connect to a
running instance of Firefox/Conkeror/Thunderbird/..., execute code directly and see the result
immediately without having to restart it.

There is also another post that I have written before that demonstrates how to
set up Mozrepl in Conkeror and integrate with Emacs here
[Mozrepl in combination with Emacs for developing Conkeror]({%post_url 2013-10-01-mozrepl-in-combination-with-emacs-for-developing-conkeror%}).
For Firefox/Thunderbird, you only need to install that extension and follow the
instruction in the **Emacs integration** section in that post.

# Beautify code

[web-beautify](https://github.com/yasuyk/web-beautify) is an Emacs interface for
[js-beautify](http://jsbeautifier.org/), an utility for formatting and indenting
Javascript, CSS and HTML file. To use it, you need **js-beautify** installed in
the background

{% highlight console %}
$ npm install -g js-beautify
{% endhighlight %}

After that, you can use Emacs
[package.el]({%post_url 2013-01-07-emacs-package-manager%}) to install
**web-beautify**. Now, everything is config automatically already for you. Just
open any js, json, css or html file and activate one of these commands
`web-beautify-js`, `web-beautify-css` or `web-beautify-html`. For more complex
beautify settings, visit the
[web-beautify](https://github.com/yasuyk/web-beautify) homepage  for more
information.

# Javascript Refactoring

The Refactoring part is now moved to a separate posts with more tools
introduced. You can access that post at
[Emacs - Javascript Completion and Refactoring]({%post_url 2014-04-20-emacs-javascript-completion-and-refactoring%})
