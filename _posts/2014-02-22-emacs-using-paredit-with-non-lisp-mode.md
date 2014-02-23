---
layout: post
title: "Emacs - Using paredit with non lisp mode"
description: ""
category: emacs
tags: [emacs, paredit]
---
{% include JB/setup %}

**Paredit** is a minor mode for keeping parenthese balanced. It is extremely
useful for working with lisp-based programming languages. Also, it is activated
automatically when you open any lisp-based languages file. Usually in other
languages, we have to work with parentheses, too. For example the **{** and
**}** for code block, **\[** and **\]** for array... To use paredit with non-lisp
mode, add the following function into your .emacs file

{% highlight cl %}
(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))
{% endhighlight %}

Actually, this is taken from Emacs Starter Kit so if you are using Emacs Starter
Kit, the above function is added already for you as `esk-paredit-nonlisp`. Emacs
Starter Kit also activates it by default for you in some modes (javascript for
example). Now, if you want to activate paredit for a certain mode, add this to
your .emacs

<!-- more -->

{% highlight cl %}
(add-hook 'js-mode-hook 'my-paredit-nonlisp) ;use with the above function
(add-hook 'js-mode-hook 'esk-paredit-nonlisp) ;for emacs starter kit
{% endhighlight %}
