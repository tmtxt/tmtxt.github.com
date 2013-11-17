---
layout: post
showtn: yes
title: "Dired as Default File Manager - Dired Async"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, file manager, dired, dired async]
---
{% include JB/setup %}

> This post is the sixth part of the series
> [Dired as Default File Manager](/2013/04/24/dired-as-default-file-manager-1-introduction/).

All this section I focus on **tmtxt-dired-async**, which is a tiny library that
I devloped to help the use of Dired become easier. Actually, the code is a bit
long so that I cannot post it here, that will make my post become messy.
Instead, I move all those functions to a separate file and post it here for easy
installation.

tmtxt-dired-async is a library for Emacs Dired mode to copy, compress,
decompress files
asynchronously. It also provides the ability to mark files in multiple
directories and then copy all of them into a destination library. This
library is designed for Unix-based system like MacOS, Ubuntu,...

The reason why I developed this library is to overcome Emacs Dired's
drawbacks. Everytime I need to copy, move, compress, uncompress,... big
files, Emacs is blocked until those processes finish execution. That's
really annoying. This extension helps solve that problem by providing
Emacs with the ability to asynchronously execute those tasks as well as
display the output to the user.

Furthermore, it also display the progress of the current task as well as notify
you when the task finish and automatically close the output window for you.

<!-- more -->

All the description, key features as well as how to install and usage are
presented on the project homepage.

**Homepage**: [http://truongtx.me/tmtxt-dired-async.html](/tmtxt-dired-async.html)

Here is some screenshots from my Emacs

![Alt Text](/files/tmtxt-dired-async/progress.png)

![Alt Text](/files/tmtxt-dired-async/finish.png)

**Previous part**:
[Dired as Default File Manager - Customize Faces](/2013/04/25/dired-as-default-file-manager-6-customize-faces/)  

