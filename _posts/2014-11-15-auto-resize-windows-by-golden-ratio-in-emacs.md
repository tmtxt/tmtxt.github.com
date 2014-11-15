---
layout: post
title: "Auto resize windows by golden ratio in Emacs"
description: ""
categories: [emacs]
tags: []
---
{% include JB/setup %}

Most of us split Emacs horizontally by 2 windows so that we can easily edit and
compare multi documents at the same time. However, the problem is that the two
windows usually have the same size and sometimes difficult for you to work with
files with long lines.
[Golden Ratio Mode](https://github.com/roman/golden-ratio.el) is a small but useful
package for Emacs that will automatically resize the focused window by
[golden ratio](http://en.wikipedia.org/wiki/Golden_ratio).

>The window that has the main focus will have the perfect size for editing,
>while the ones that are not being actively edited will be re-sized to a smaller
>size that doesn't get in the way, but at the same time will be readable enough
>to know it's content.

You can install it using [package.el]({%post_url 2013-01-07-emacs-package-manager%}).
After finishing installation, just add this to your init.el file

{% highlight cl %}
(require 'golden-ratio)
(golden-ratio-mode 1)
{% endhighlight %}

<!-- more -->

To prevent Golden Ratio mode to be activated for certain mode, use this

{% highlight cl %}
(setq golden-ratio-exclude-modes '("ediff-mode"
                                   "eshell-mode"
                                   "dired-mode"))
{% endhighlight %}

On a high resolution screen, when the display width of the focused window resized by
golden ratio is larger than 160 characters, opening any commands which call to
`pop-up-window` will cause Emacs to create extra window instead of jump to an
already existing window. TO prevent that, just set the variable
`split-width-threshold` to nil

{% highlight cl %}
(setq split-width-threshold nil)
{% endhighlight %}

![Golden Ratio](/files/2014-11-15-auto-resize-windows-by-golden-ratio-in-emacs/golden.gif )
