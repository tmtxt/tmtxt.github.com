---
layout: post
title: "Dired as Default File Manager - More Advanced Tips"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, dired, file manager]
---
{% include JB/setup %}

> This post is the third part of the series
> [Dired as Default File Manager](/2013/04/24/dired-as-default-file-manager-1-introduction/).

# Mark file backward

To mark file backward (mark file and move the cursot back to the previous line,
opposite with when you press m), add this to your .emacs. If you just want the
cursor to stay at the
current file, remove **(call-interactively 'dired-previous-line)** in the code.
Change s-b to the key binding that you like

<!-- more -->

{% highlight cl %}
(defun dired-mark-backward ()
  (interactive)
  (call-interactively 'dired-mark)
  (call-interactively 'dired-previous-line)
  (call-interactively 'dired-previous-line))
(define-key dired-mode-map (kbd "s-b") 'dired-mark-backward)
{% endhighlight %}

# Open files by default programs

Both functions on MacOS and Ubuntu are bounded to s-o. You can mark multiple
files and then s-o to open it using the default program on Mac OS.

**Mac OS**: Source:
<http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/>  

{% highlight cl %}
(defun dired-do-shell-mac-open-vqn ()
(interactive)
(save-window-excursion
 (dired-do-async-shell-command
 "open" current-prefix-arg
 (dired-get-marked-files t current-prefix-arg))))
(define-key dired-mode-map (kbd "s-o") 'dired-do-shell-mac-open-vqn)
{% endhighlight %}

**Ubuntu**: Source:
<https://github.com/ubolonton/.emacs.d/blob/master/config/ublt-dired.el>  
You need to replace **gnome-open** with your open command in your OS.

{% highlight cl %}
(defun ublt/dired-open-native ()
  "Open marked files (or the file the cursor is on) from dired."
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg))
         (n (length files)))
    (when (or (<= n 3)
              (y-or-n-p (format "Open %d files?" n)))
      (dolist (file files)
        (call-process "gnome-open"
                      nil 0 nil file)))))
(define-key dired-mode-map (kbd "s-o") 'ublt/dired-open-native)
{% endhighlight %}

# Unmount disk in Dired

Source:
<http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/>  
Both functions on MacOS and Ubuntu are bounded to s-u.

**MacOS**

{% highlight cl %}
(defun dired-do-shell-unmount-device ()
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "diskutil unmount" current-prefix-arg
	   (dired-get-marked-files t current-prefix-arg))))
(define-key dired-mode-map (kbd "s-u") 'dired-do-shell-unmount-device)
{% endhighlight %}

**Ubuntu**

{% highlight cl %}
(defun dired-do-shell-unmount-device ()
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "umount" current-prefix-arg
	   (dired-get-marked-files t current-prefix-arg))))
(define-key dired-mode-map (kbd "s-u") 'dired-do-shell-unmount-device)
{% endhighlight %}

# Open current directory in default file manager

**MacOS**

{% highlight cl %}
(tmtxt/in '(darwin)
  (defun tmtxt/dired-open-current-directory-in-finder ()
	"Open the current directory in Finder"
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "open ."))))
(define-key dired-mode-map (kbd "s-O") 'tmtxt/dired-open-current-directory-in-finder)
{% endhighlight %}

**Ubuntu** (hasn't been checked yet)
You need to replace **gnome-open** with your open command in your OS.

{% highlight cl %}
(tmtxt/in '(darwin)
  (defun tmtxt/dired-open-current-directory ()
	"Open the current directory in Finder"
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "gnome-open ."))))
(define-key dired-mode-map (kbd "s-O") 'tmtxt/dired-open-current-directory)
{% endhighlight %}

It's bounded to s-O. Pressing s-O in a dired buffer will cause your default file
manager to open the current directory. In other type of buffer, such as file
buffer, you can call this function interactively by M-x and
tmtxt/dired-open-current-directory-in-finder (MacOS) or
tmtxt/dired-open-current-directory (Ubuntu). In that case, the default file
manager application on your computer will open the directory that contains the
current file.

**Previous part**:
[Dired as Default File Manager - Dired Details](/2013/04/24/dired-as-default-file-manager-3-dired-details/)  
**Next part**: [Dired as Default File Manager - Customize ls command](/2013/04/25/dired-as-default-file-manager-5-customize-ls-command/)
