---
layout: post
title: "Emacs - Setup JSX mode, JSX Syntax checking and Suggestion"
description: ""
categories: [emacs, javascript]
tags: [emacs, jsx, jsxhint]
---


> **Update Jun 21 2014**: add another better solution that uses web-mode

# 1. Syntax highlighting

If you are working with Javascript, especially ReactJS, you definitely have known
about JSX, the XML syntax inside of Javascript. This section will demonstrate how
to setup Emacs for working with JSX files using [web-mode](http://web-mode.org/
), an autonomous
emacs major-mode for editing web templates. For web-mode to work properly with
JSX files, add this to your .emacs

{% highlight cl %}
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))
{% endhighlight %}

[This](/files/2014-03-10-emacs-setup-jsx-mode-and-jsx-syntax-checking/jsx.html)
is how web-mode indents and highlights my jsx files
(taken from my Emacs).

# 2. JSX Syntax Checking

![Alt Text](/files/2014-03-10-emacs-setup-jsx-mode-and-jsx-syntax-checking/flycheck.png)

Next, we need a tool for syntax checking. For Javascript, we have `jslint` (or
`jshint`), which I have previously written another post about how to set it up
in Emacs here
[Emacs - Setup JSHint for on-the-fly (potential) errors checking]({%post_url 2014-02-21-emacs-setup-jshint-for-on-the-fly-petential-error-checking%}).
For JSX, there is a similar tool called `jsxhint`. Of course, you need to
install it before you can use

<!-- more -->

{% highlight console %}
$ npm install -g jsxhint
{% endhighlight %}

You can test whether the tool works correctly by trying to check the syntax of a
JSX file

{% highlight console %}
$ jsxhint example.jsx
{% endhighlight %}

Now, to integrate it into Emacs, you need to install `flycheck` (a modern
version of `flymake`) through Emacs Packages Manager. Now, I will show 2 ways of
setting it corresponding to the 2 solutions that I mentioned in part 1 (web-mode
and jsx-mode).

**Note**: you need to make sure that `jsxhint` is located inside your Emacs'
path (or install `exec-path-from-shell` to import your shell's PATH
automatically).

Add this to your .emacs to activate the checker for .jsx files

{% highlight cl %}
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))
{% endhighlight %}

# JSX Auto complete, Snippets and Suggestion

Finally, the greatest thing when using Emacs for JS development is the
Suggestion feature with [TernJS](http://ternjs.net/). The steps on how to set up
TernJS for both JS and JSX mode is presented in this post
[Emacs - Javascript Completion and Refactoring]({%post_url 2014-04-20-emacs-javascript-completion-and-refactoring%}).
