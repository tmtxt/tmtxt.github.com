---
layout: post
showtn: yes
title: "Config Yasnippet and Autocomplete on Emacs"
description: ""
category: Emacs
thumbnail: /files/2013-01-06-config-yasnippet-and-autocomplete-on-emacs/thumbnail.png
tags: [yasnippet, autocomplete, emacs]
---
{% include JB/setup %}

This is a simple way to make yasnippet and autocomplete work together on emacs.
Both of them use the **Tab** key to activate. Everytime you press **Tab**,
yasnippet will run first. If there is no snippet found, autocomplete will be
activated.

Download **yasnippet** and **autocomplete** and add them to the load path or just
simply install them using **package.el**. After that, add this to your **.emacs**

First, activate yasnippet.

<!-- more -->

{% highlight cl %}
;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)
{% endhighlight %}

Next, activate and config autocomplete. Put it after the code activating yasnippet.

{% highlight cl %}
;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
{% endhighlight %}

Restart emacs or **eval-region** to apply changes.
