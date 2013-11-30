---
layout: post
title: "Relase of tmtxt-async-tasks and Update of tmtxt-dired-async"
description: ""
category: emacs
thumbnail: 
showtn: no
tags: [emacs]
---
{% include JB/setup %}

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

# Update of tmtxt-dired-async

With the release of *tmtxt-async-tasks*, *tmtxt-dired-async* should be updated,
too. There is no need to keep all the code to executing command asynchronously
inside *tmtxt-dired-async* because the new package has all that feature.
*tmtxt-dired-async* now relies on *tmtxt-async-tasks* so if you are going to
update to the new version, please include *tmtxt-async-tasks* as well. Also, in
the new release, I have changed many function name to make it shorter and
changed many option variables for easier usage
so if you want to upgrade, please change the function name accordingly.
You can see all the new name at the project's homepage
[http://truongtx.me/tmtxt-dired-async.html](/tmtxt-dired-async.html).

<!-- more -->
