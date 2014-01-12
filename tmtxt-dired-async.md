---
layout: page
title: "tmtxt-dired-async"
description: "An extension for Emacs Dired to execute tasks asynchronously"
group: project
---
{% include JB/setup %}

# Asynchoronous execution library for Emacs Dired

A library for Emacs Dired mode to copy, compress, decompress files
asynchronously. It also provides the ability to mark files in multiple
directories and then copy all of them into a destination library. This
library is designed for Unix-based system like MacOS, Ubuntu,... The
reason why I developed this library is to overcome Emacs Dired's
drawbacks. Everytime I need to copy, move, compress, uncompress,... big
files, Emacs is blocked until those processes finish execution. That's
really annoying. This extension helps solve that problem by providing
Emacs with the ability to asynchronously execute those tasks as well as
display the output to the user.

# Installation

**tmtxt-dired-async** replies on **tmtxt-async-tasks** so before using it,
please install [tmtxt-async-tasks](/tmtxt-async-tasks.html) first.

After that, clone this repo and put it somewhere in your load-path  
Add this to your .emacs

{% highlight cl %}
(require 'tmtxt-dired-async)
{% endhighlight %}

# Features

You don't have to follow the key bindings below, you can change them to whatever
you want. They are just examples.

## Asynchronously Copy/Sync files

This feature uses `rsync` for file copying. To use it, simply mark the files
that you want and then activate this function.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-r") 'tda/rsync)
{% endhighlight %}

To `rsync` with `--delete` option to create an exact copy of the source files
(synchronize files), use this function

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-r") 'tda/rsync-delete)
{% endhighlight %}

Sometimes **rsync** need sudo permission for preserving file attributes. In that
case, there are 2 alternative commands for the above ones. They are
**tda/rsync-sudo** and **tda/rsync-delete-sudo**.

If your rsync program is outside of the **PATH**, set the path to the rsync
executable for this variable

{% highlight cl %}
(set-default tda/rsync-command-name "/path/to/rsync")
{% endhighlight %}

To change the arguments passed into rsync command, set it for this variable. The
default is `-avz --progress`.

{% highlight cl %}
(set-default tda/rsync-arguments "-avz --progress")
{% endhighlight %}

## Asynchronously Compress files

Compress all marked files.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-z") 'tda/zip)
{% endhighlight %}

If you want to use another `zip` command, not the system's default one, set the
path to executable file and it arguments for these 2 variables

{% highlight cl %}
(setq-default tda/zip-command "/path/to/zip/command")
(setq-default tda/zip-argument "-ru9")
{% endhighlight %}

## Asynchronously Decompress files

Decompress the zip file at point.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-u") 'tda/unzip)
{% endhighlight %}

If you want to use another `unzip` command, not the system's default one, set the
path to executable file and it arguments for these 2 variables

{% highlight cl %}
(setq-default tda/unzip-command "/path/to/unzip/command")
(setq-default tda/unzip-argument "")
{% endhighlight %}

## Copy from multiple directories

This feature allows you to select many files from multi directories and then
copy all of them to a destination folder.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-a") 'tda/rsync-multiple-mark-file)
(define-key dired-mode-map (kbd "C-c C-e") 'tda/rsync-multiple-empty-list)
(define-key dired-mode-map (kbd "C-c C-d") 'tda/rsync-multiple-remove-item)
(define-key dired-mode-map (kbd "C-c C-v") 'tda/rsync-multiple)
{% endhighlight %}

**C-c C-a** to add the file at point to the list for later copy. **C-c C-d** to
remove the current file at point from the waiting list. **C-c C-e** to empty the
waiting list. Finally, **C-c C-v** to copy all files in the list to the current
directory.

## Get size of multiple files

This feature uses the command `du` to calculate the total size of all marked
files.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-s") 'tmtxt/dired-async-get-files-size)
{% endhighlight %}

If you want to use another `unzip` command, not the system's default one, set the
path to executable file and it arguments for these 2 variables

{% highlight cl %}
(setq-default tda/get-file-size-command "/path/to/du/command")
(setq-default tda/get-files-size-arguments "-hc")
{% endhighlight %}

## Download file to current dir

This command read input link from minibuffer and then download it to the current
directory.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-q") 'tda/download-to-current-dir)
{% endhighlight %}

Another handy command is `tda/download-clipboard-to-current-dir`, which is
similar to the above command. Instead of prompting user for the link, it reads
from the clipboard.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-l") 'tda/download-clipboard-link-to-current-dir)
{% endhighlight %}

You can also specify the download program you want to use by changing the
variable `tda/download-command`. The default value is **wget**, you can change
it to **curl**, **aria2c**,...

{% highlight cl %}
(setq tda/download-command "wget")
{% endhighlight %}

**Note**: On Mac OS, if your wget encounter the problem with ssl certificate,
read the instruction in this
[post]({%post_url 2014-01-12-macos-fix-wget-certificate-error%}).
