---
layout: post
title: "tmtxt dired async - Update"
description: ""
category: Emacs
thumbnail: 
showtn: yes
tags: [emacs, dired async, dired, file manager]
---
{% include JB/setup %}

> Today, I have released some minor update for **tmtxt dired async** library (an
> asynchronous execution library for emacs dired).
> 
> [tmtxt dired async Homepage](/tmtxt-dired-async.html)
> 
> The update fixes some minor issues and add some more features as below

# Get size of multiple files

This feature uses the command `du` to calculate the total size of all marked
files.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-s") 'tmtxt/dired-async-get-files-size)
{% endhighlight %}

# Stop all current async tasks

This function helps you stop all currently running async taks.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-k") 'tmtxt/dired-async-kill-all)
{% endhighlight %}

<!-- more -->

# Jump to the end of the output buffer

Sometimes, the point in the output buffer stucks somewhere in the middle of the
output buffer and will not auto scroll for user to track the progress. Activate
this function to fix it.

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-n") 'tmtxt/dired-async-move-all-points-to-end)
{% endhighlight %}
