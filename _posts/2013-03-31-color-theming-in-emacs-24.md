---
layout: post
showtn: yes
title: "Color Theming in Emacs 24"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, theme]
---
{% include JB/setup %}

# Introduction

Emacs is an extremely extensibile and customizable text editor. We can customize
nearly every aspect of it. Color theming, of course, is not an exception in
Emacs. A good color theming can make us feel more pleasant and literally
improves our work.

Since version 24, Emacs offers a
powerful tool to create and maintain custom theme called
**customize-create-theme**. It provides you an visual interface for interacting
with
all those facets as well as previewing color in Emacs. You can even copy from
other theme and then customize it a bit to make it yours. Here is some tips on
how to create a custom theme with Emacs 24.

<!-- more -->

# Create Custom Color Theming in Emacs 24

Emacs 24 provides a new function named **deftheme** to define a new theme.
However, there is no need to manually
program you custom theme and deal with the crazy color codes. Just open
your Emacs, hit `M-x`, run the command `customize-create-theme`. It will ask
you wether you want to include basic face customizations in the new theme. You
can choose `y` for it create some basic facets for you. If you want to copy
from other theme, head over to the beginning of the buffer and select the
`Visit Theme` button. A new window will appear for you to choose which theme
you want to copy from.

![Emacs Theme Creation Interface](/files/2013-03-31-color-theming-in-emacs-24/theme.png)  
Emacs 24 Theme Creation Interface

As you can see in the picture above, Emacs offer a visual interface for you to
edit and define how things should look like. You can even add Addition Faces and
Variables into you theme by selecting the **Insert Additional Face** and
**Insert Variable** at the end of the buffer. After finishing creating your
custom them, enter the theme name and description, select the **Save Theme**
button to save your theme. By default, the newly created theme will be store in
*~/.emacs.d/* with the name *themename-theme.el*. If you want it to store into a
different directory, add this to your .emacs. Replace *your-directory* with the
directory that you want.

{% highlight cl %}
(setq custom-theme-directory "your-directory")
{% endhighlight %}

# Load Custom Theme

To activate your custom theme, add this to your .emacs or simply just evaluate it

{% highlight cl %}
(add-to-list 'custom-theme-load-path "your-directory")
(load-theme 'theme-name t)
{% endhighlight %}

Replace *your-directory* with the path to your folder that contains the .el
theme file. Also, replace *theme-name* with you custom theme name.

# Misc

[Rainbow mode](http://julien.danjou.info/projects/emacs-packages#rainbow-mode )
is a useful package for developing emacs theme. You can find and install it
using Emacs package manager. Once activate, it will automatically convert all
strings that respresent color to the corresponding color and display it.
However, if you use the built in Emacs 24 theme creator, you may not need to
install this package.

![Rainbow mode](/files/2013-03-31-color-theming-in-emacs-24/rainbow.png )  
Rainbow mode screenshot

The Emacs Package Manager also provide you a lot of custom themes from the
contribution of the Emacs user community. You can pick the one that you like,
install it and then custom it to your need. (I recomended solarized color
theme).

# Conclusion

In summary, **customize-create-theme** is really a powerful tool that comes with
Emacs 24 for helping you to create your own theme. There is now no fear of
dealing with those complicated and crazy color code. Everything is automated. If
you found it too hard to develop a theme in Emacs 23 and prior, it's time for
you to develop one for your own.
