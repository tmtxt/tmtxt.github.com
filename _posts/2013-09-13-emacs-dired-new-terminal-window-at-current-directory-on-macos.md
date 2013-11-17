---
layout: post
title: "Emacs Dired - New Terminal window at current directory on MacOS"
description: ""
category: Emacs
thumbnail: 
showtn: no
tags: [emacs, dired, terminal, macos]
---
{% include JB/setup %}

Although I'm using emacs dired as my default file manager, I don't like any
of the terminal emulators in Emacs at all. I usually use a separate application
for working with shell command (iTerm on Mac). The problem is that
when I'm browsing my files in Emacs and I want to open terminal for doing shell
commands in the current directory, I have to copy the path of the current
directory, open terminal and type cd to that folder.

I've written a small function
to help me quickly open a new terminal window at the current directory
that I'm browsing in Emacs (MacOS only).

{% highlight cl %}
;; default terminal application path
(defvar tmtxt/macos-default-terminal-app-path
	"/Applications/Terminal.app" "The default path to terminal application in MacOS")
;;; function to open new terminal window at current directory
  (defun tmtxt/open-current-dir-in-terminal ()
	"Open current directory in dired mode in terminal application.
For MacOS only"
	(interactive)

	(shell-command (concat "open -a "
						   (shell-quote-argument tmtxt/macos-default-terminal-app-path)
						   " "
						   (shell-quote-argument (file-truename default-directory)))))
{% endhighlight %}

<!-- more -->

When you want to use the function, you need to set the variable
**tmtxt/macos-default-terminal-app-path** to the absolute path to your terminal
application if you're not using MacOS's default terminal application.

{% highlight cl %}
(setq-default tmtxt/macos-default-terminal-app-path "/Volumes/tmtxt/Applications/iTerm.app")
{% endhighlight %}

Finally, bind it to a keystroke that you want, for example

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-o") 'tmtxt/open-current-dir-in-terminal)
{% endhighlight %}

Now in any dired buffer, just type **C-c C-o**, the terminal application that you
specified before will open and automatically cd to the current directory. In any
other buffer types, you can still activate this function by using **M-x**, the
terminal application will open with the directory that contain the file you're
currently editing.
