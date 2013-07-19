---
layout: post
showtn: yes
title: "Install and Config ECB on Emacs"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [ecb, emacs]
---
{% include JB/setup %}

# Introduction

ECB, stands for Emacs Code Browser, tranforms your emacs from a text editor to a
real IDE for coding. Once activated, it can display many useful information that
help you program more effectively. The ECB's informational windows can contain:

* a directory tree,
* a list of source files in the current directory,
* a list of functions/classes/methods/... in the current file, (ECB uses
  the Semantic Bovinator, or Imenu, or etags, for getting this list so all
  languages supported by any of these tools are automatically supported by
  ECB too)
* a history of recently visited files,
* the Speedbar and
* output from compilation (the "*compilation*" window) and other modes like
  help, grep, etc. or whatever the user defines to be displayed in that
  window.

For more information, please visit [ECB Homepage](http://ecb.sourceforge.net/).

<!-- more -->

# Screenshot

Here is the ECB screenshot from my Emacs

![ECB Screenshot](/files/2013-03-10-ecb---emacs-code-browser/ecb.png )

# Installation

ECB requires CEDET version 1.0+, which is included in Emacs version 24.

You can install ECB manually by cloning the package and add it to your
load-path. However, there is another convinience method, that is to use
package.el. If you haven't known it yet, have a look at this post
[Emacs Package Manager](/2013/01/07/emacs-package-manager/ ). To install it, `M-x`
and type in `list-package` `RET`, `C-s` and search for **ecb**, `RET` and select **Install**.
After that, add this to your .emacs or init.el file

{% highlight cl %}
;;; activate ecb
(require 'ecb)
(require 'ecb-autoloads)
{% endhighlight %}

Now, everything you need to run ECB is to type `M-x` `ecb-activate` `RET` and ECB is now
ready for you to use. When you want to stop it, just execute the command
`ecb-deactivate`.

# Some Basic Configurations

To set the layout for ECB, add this to your .emacs

{% highlight cl %}
(setq ecb-layout-name "layout-name")
{% endhighlight %}

Replace layout-name with the name of the layout you want. For a list of ECB
layouts, follow this link
[ECB Layouts](http://ecb.sourceforge.net/docs/Changing-the-ECB-layout.html ).

Show source files in directories buffer

{% highlight cl %}
(setq ecb-show-sources-in-directories-buffer 'always)
{% endhighlight %}

By default, ECB hides the compilation window. Everytime I compile, it displays
the compilation message to the same window as the buffer that I'm editing.
This is really annoying because I want to see all the messages so that I can fix
the error in my code easily. To keep a persistent compile window in ECB, add
this to your .emacs

{% highlight cl %}
(setq ecb-compile-window-height 12)
{% endhighlight %}

Some key bindings for quick interation with ECB, replace them with the key
bindings that you want.

{% highlight cl %}
;;; activate and deactivate ecb
(global-set-key (kbd "C-x C-;") 'ecb-activate)
(global-set-key (kbd "C-x C-'") 'ecb-deactivate)
;;; show/hide ecb window
(global-set-key (kbd "C-;") 'ecb-show-ecb-windows)
(global-set-key (kbd "C-'") 'ecb-hide-ecb-windows)
;;; quick navigation between ecb windows
(global-set-key (kbd "C-)") 'ecb-goto-window-edit1)
(global-set-key (kbd "C-!") 'ecb-goto-window-directories)
(global-set-key (kbd "C-@") 'ecb-goto-window-sources)
(global-set-key (kbd "C-#") 'ecb-goto-window-methods)
(global-set-key (kbd "C-$") 'ecb-goto-window-compilation)
{% endhighlight %}

# Some of My Functions

Personally, I don't like those function `ecb-deactivate`, `ecb-show-ecb-windows`,
`ecb-hide-ecb-windows` since I usually have to open multiple windows in Emacs. When
deactivate or hide ECB window, it does not automatically restore the window
layout and switch to the buffer that I'm editing. Similar problem happens when I
try to show ecb window again. Because of that, I wrote those functions to
replace the ECB built in three functions ecb-deactivate, ecb-show-ecb-windows,
ecb-hide-ecb-windows.

{% highlight cl %}
;;; replacement for built-in ecb-deactive, ecb-hide-ecb-windows and
;;; ecb-show-ecb-windows functions
;;; since they hide/deactive ecb but not restore the old windows for me
(defun tmtxt/ecb-deactivate ()
  "deactive ecb and then split emacs into 2 windows that contain 2 most recent buffers"
  (interactive)
  (ecb-deactivate)
  (split-window-right)
  (switch-to-next-buffer)
  (other-window 1))
(defun tmtxt/ecb-hide-ecb-windows ()
  "hide ecb and then split emacs into 2 windows that contain 2 most recent buffers"
  (interactive)
  (ecb-hide-ecb-windows)
  (split-window-right)
  (switch-to-next-buffer)
  (other-window 1))
(defun tmtxt/ecb-show-ecb-windows ()
  "show ecb windows and then delete all other windows except the current one"
  (interactive)
  (ecb-show-ecb-windows)
  (delete-other-windows))
{% endhighlight %}

After that, bind them with a key sequence for quick interaction with ECB

{% highlight cl %}
(global-set-key (kbd "C-x C-'") 'tmtxt/ecb-deactivate)
(global-set-key (kbd "C-;") 'tmtxt/ecb-show-ecb-windows)
(global-set-key (kbd "C-'") 'tmtxt/ecb-hide-ecb-windows)
{% endhighlight %}
