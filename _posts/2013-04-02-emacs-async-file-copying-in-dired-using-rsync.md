---
layout: post
showtn: yes
title: "Emacs - Async File Copying in Dired using Rsync"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, dired, rsync]
---
{% include JB/setup %}

> **Update**: I have implemented a better version of this code. The new version is
> able to display progress separately and auto hide the rsync window after
> finishing copying. You can look at this post
> [Emacs - Async File copying with Rsync - Update](/2013/04/06/emacs-async-file-copying-with-rsync-update/)

Emacs Dired mode is a great alternative for the default file explorer
application on
MacOS, Windows or other systems. That is because Dired mode provides with
the ability to quickly navigate through files and directories, mark
multiple files for later operation using keyboard. However, when dealing with
large and multiple files, it show one annoying drawback. When we want to copy or
move a big file, Emacs will be entirely blocked until the copying/renaming
process complete.

<!-- more -->

There are several methods to fix this problem. My solution is to replace copy
command with **rsync** and execute it asynchronously so that the rsync process
can execute in the background while you can still interact with Emacs normally.
Moreover, **rsync** can also display the progress of the process (the percentage
of file copied) as well as determine which files are already exist to avoid
duplicated copying (since its main purpose is to sync).

Below is my solution. If you want to use it, simply copy and paste it into your
.emacs and restart Emacs for changes to take effect.

**Note**: this piece code is only compatible with those unix-based systems
because they are usually equipped with rsync application by default.

{% highlight cl %}
(defun tmtxt/dired-rsync (dest)
  (interactive
   ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files nil current-prefix-arg)))
	;; the rsync command
	(setq tmtxt/rsync-command "rsync -arvz --progress ")
	;; add all selected file names as arguments to the rsync command
    (dolist (file files)
	  (setq tmtxt/rsync-command
			(concat tmtxt/rsync-command
					(shell-quote-argument file)
					" ")))
	;; append the destination
	(setq tmtxt/rsync-command
		  (concat tmtxt/rsync-command
				  (shell-quote-argument dest)))
	;; run the async shell command
	(async-shell-command tmtxt/rsync-command "*rsync*")
	;; finally, switch to that window
	(other-window 1)))
;;; bind it to C-c C-r
(define-key dired-mode-map (kbd "C-c C-r") 'tmtxt/dired-rsync)
{% endhighlight %}

I also bound it to C-c C-r, if you want, you can change it into another key
binding that you want.

After restarting Emacs, you are ready to use that new function. Open up a Dired
buffer, mark some files and then C-c C-r, select the destination directory and
press RET. If you have the variable dired-dwim-target set to t, you can open 2
dired windows and this command will automatically choose the directory in the
other window as the default destination folder.

**Screen shot from my Emacs**:

![Rsync3](/files/2013-04-02-emacs-async-file-copying-in-dired-using-rsync/rsync3.png)  
Rsync process with progress.

![Rsync2](/files/2013-04-02-emacs-async-file-copying-in-dired-using-rsync/rsync2.png)  
Rsync process with progress.

![Rsync1](/files/2013-04-02-emacs-async-file-copying-in-dired-using-rsync/rsync1.png)  
Rsync process when complete.

**Note**: You should only use this function when you want to copy large files.
For the small ones, just use the default copy command of dired because this
command always open a new buffer for showing the progress.
