---
layout: post
title: "Setup PHP Development Environment in Emacs using web-mode"
description: ""
categories: [emacs]
tags: []
---
{% include JB/setup %}

# web-mode for PHP coding

PHP code are usually mixed inside HTML markup code. It's very hard for us to use
only `php-mode` to indent both php code and markup code. However, with the help
of `web-mode`, an autonomous emacs major-mode for editing web templates, you can
achieve it easily. Here is the comparison pictures between 2 modes.

- php-mode

![php-mode](/files/2014-07-22-setup-php-development-environment-in-emacs/php.png)

<!-- more -->

- web-mode

![web-mode](/files/2014-07-22-setup-php-development-environment-in-emacs/web.png)

As you can see from the above pictures, `php-mode` can indents only in php code,
not the markup code, which make the file become very ugly and hard to read while
`web-mode` can distinguish between markup block and php block.
`web-mode` also allows you to specify different indentation level for each type of
code block (markup, css, php). Also, Emacs' built-in function
`make-local-variable` helps you to set the indentation level for only .php file
without affecting other kinds of file that also use `web-mode`.

# Install and Config web-mode for PHP

First, you need to install `web-mode`, either using
[package.el]({%post_url 2013-01-07-emacs-package-manager%}) or cloning it by
yourself. You can also install `emmet-mode` for faster markup snippet insertion.

Next, add a function for setting up web-mode for php files

{% highlight cl %}
(defun my-setup-php ()
  ;; enable web mode
  (web-mode)

  ;; make these variables local
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)

  ;; set indentation, can set different indentation level for different code type
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))
{% endhighlight %}

Next, call that function when open any .php file

{% highlight cl %}
(add-to-list 'auto-mode-alist '("\\.php$" . my-setup-php))
{% endhighlight %}

# Syntax Checking for PHP files

For php syntax checking, you need to install php command line tool and an Emacs
package called `flycheck` (can be installed through package.el, too). On Linux,
you can easily install it using your system's package manager. On OSX, the tool
is installed already for you (Apple did that) or you can install it manually
(for newer version) using Macports. Make sure that `php` command line executable
is inside you Emacs
load path (or install `exec-path-from-shell` package to import the PATH from
your shell automatically).

Flycheck already includes config for checking PHP syntax using php command line.
However, the checker is limited for using with **php-mode** and **php+mode**
only. To use it with web-mode, you need to re-define it by yourself

{% highlight cl %}
(flycheck-define-checker my-php
  "A PHP syntax checker using the PHP command line interpreter.

See URL `http://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0" source)
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode web-mode))
{% endhighlight %}

This is just the code that I copy from flycheck source code. The only change
that I made is to add `web-mode` to the allow modes list (the last line).

Next, come back to the `my-setup-php` function that we have defined before. Add
these two lines to the end of the function body to activate the new checker
automatically for all .php files

{% highlight cl %}
(flycheck-select-checker my-php)
(flycheck-mode t))
{% endhighlight %}

![Flycheck](/files/2014-07-22-setup-php-development-environment-in-emacs/error.png)
