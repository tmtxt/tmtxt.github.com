---
layout: page
title: "tmtxt-dired-async"
description: "An extension for Emacs Dired to execute tasks asynchronously"
group: project
---
{% include JB/setup %}

> For discussing, Q&A, please contact me via the email or comment in this
> post  
> [Dired as Default File Manager - Dired Async]({% post_url 2013-04-25-dired-as-default-file-manager-7-dired-async %})

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

Clone this repo and put it somewhere in your load-path  
Add this to your .emacs

{% highlight cl %}
(require 'tmtxt-dired-async)
{% endhighlight %}

# Features

You don't have to follow the key bindings below, you can change them to whatever
you want. They are just examples.

## Asynchronously Copy files

This feature uses `rsync` for file copying. To use it, simply mark the files
that you want and then activate this function.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-r") 'tmtxt/dired-async-rsync)
{% endhighlight %}

Show the progress when copy (nil to not show).

{% highlight cl %}
(setq-default tmtxt/dired-async-rsync-show-progress t)
{% endhighlight %}

Show verbosity when copy (nil to not show).

{% highlight cl %}
(setq-default tmtxt/dired-async-rsync-show-verbosity t)
{% endhighlight %}

Use archive mode when copy (to preserve time stamp, nil to not use)

{% highlight cl %}
(setq-default tmtxt/dired-async-rsync-archive-mode t)
{% endhighlight %}

User compression mode when copy (nil to not use).

{% highlight cl %}
(setq-default tmtxt/dired-async-rsync-compress-mode t)
{% endhighlight %}

## Asynchronously Copy files (with delete option)

This feature is similar to the above feature. It also uses `rsync` to copy
file and the same config with the above. However, it includes the delete option
for rsync to ensure that you have the destination folder exactly the same as the
source directory

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-t") 'tmtxt/dired-async-rsync-delete)
{% endhighlight %}

Set the deletion method for rsync delete (--delete-after, --delete-during, --delete-before)

{% highlight cl %}
(setq-default tmtxt/dired-async-rsync-delete-method "--delete-after")
{% endhighlight %}

## Asynchronously Compress files

Compress all marked files.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-z") 'tmtxt/dired-async-zip)
{% endhighlight %}

Set the compression level, from 0-9

{% highlight cl %}
(setq-default tmtxt/dired-async-zip-compression-level "9")
{% endhighlight %}

## Asynchronously Decompress files

Decompress the zip file at point.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-u") 'tmtxt/dired-async-unzip)
{% endhighlight %}

## Copy from multiple directories

This feature allows you to select many files from multi directories and then
copy all of them to a destination folder.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-a") 'tmtxt/dired-async-rsync-multiple-mark-file)
(define-key dired-mode-map (kbd "C-c C-e") 'tmtxt/dired-async-rsync-multiple-empty-list)
(define-key dired-mode-map (kbd "C-c C-d") 'tmtxt/dired-async-rsync-multiple-remove-item)
(define-key dired-mode-map (kbd "C-c C-v") 'tmtxt/dired-async-rsync-multiple)
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

## Stop all current async tasks

This function helps you stop all currently running async taks.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-k") 'tmtxt/dired-async-kill-all)
{% endhighlight %}

## Jump to the end of the output buffer

Sometimes, the point in the output buffer stucks somewhere in the middle of the
output buffer and will not auto scroll for user to track the progress. Activate
this function to fix it.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-n") 'tmtxt/dired-async-move-all-points-to-end)
{% endhighlight %}

## Other config

Set the time to close the result window after finish, measured in second

{% highlight cl %}
(setq-default tmtxt/dired-async-post-process-window-show-time "5")
{% endhighlight %}

Set the height for the result window, measured in the number of lines

{% highlight cl %}
(setq-default tmtxt/dired-async-result-window-height 10)
{% endhighlight %}
