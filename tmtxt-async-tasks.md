---
layout: page
title: "tmtxt-async-tasks"
description: "An extension for Emacs to execute tasks asynchronously"
group: project
---

# tmtxt-async-tasks

This is a new emacs package that I've just finished. This is not a very
complicated extension or something very new. This is just the code I extracted
from my previous emacs' extension *tmtxt-dired-async*. The purpose of
*tmtxt-async-tasks* is to provide emacs users a tool for executing shell command
asynchronously in a separate window and not be affected by the process. When you
executing a command using this package, a new window will be created at the
bottom of the current frame (this window is not affected by the *other-window*
command). The shell command keep running and print out the result for you to
keep track of the process and the window contains that command can be
automatically closed after a specified time period when it finish executing. For
more information, demo as well as how to use the package, please visit it's
homepage at
[http://truongtx.me/tmtxt-async-tasks.html](/tmtxt-async-tasks.html).

![Screenshot](/files/tmtxt-async-tasks/ss.png)

# Installation

To install the package, simply clone the package from github

{% highlight console %}
$ git clone https://github.com/tmtxt/tmtxt-async-tasks.git
{% endhighlight %}

Put it somewhere in your emacs' load-path and load it into emacs using this
command

{% highlight cl %}
(require 'tmtxt-async-tasks)
{% endhighlight %}

# Interactive Usage

To use it interactively, activate the command name
**tat/execute-async-interactive** and type in the command that you want to
execute. The output of the command will be printed into the new window at the
bottom. After finishing execution, it will wait for 5s (can be changed) and
then close the window automatically.

# Stop all current async tasks

This function helps you stop all currently running async tasks. The function
name is `tat/kill-all`

# Jump to the end of the output buffer

Sometimes, the point in the output buffer stucks somewhere in the middle of the
output buffer and will not auto scroll for user to track the progress. Activate
this function to fix it.

{% highlight cl %}
(global-set-key (kbd "C-c C-n") 'tat/move-to-bottom-all)
{% endhighlight %}

# Using in your code

In you code, if you want to to execute a shell command, call the function
**tat/execute-async**. The first argument is the command to execute. The second
one is the name to display. For example

{% highlight cl %}
(tat/execute-async "du -hc ~/" "get file size")
{% endhighlight %}

# Some options

To set the delay (in second) for the output window before closing, change the variable
**tat/window-close-delay**. Remember to set it as a string.

{% highlight cl %}
(setq-default tat/window-close-delay "5")
{% endhighlight %}

To set the height of the output window, set the variable **tat/window-height**.
The height is measured by the number of lines.

{% highlight cl %}
(setq-default tat/window-height 10)
{% endhighlight %}

# For using with Dired

I have another package that relies on this package for using with Dired to copy,
synchronize, compress, decompress,... files. If you want, visit it home page at
[tmtxt-dired-async](/tmtxt-dired-async.html).
