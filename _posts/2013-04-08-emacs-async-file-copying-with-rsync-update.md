---
layout: post
showtn: yes
title: "Emacs - Async File copying with Rsync - Update Apr 8 2013"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, rsync, dired]
---
{% include JB/setup %}

In this previous
[Emacs - Async File Copying in Dired using Rsync](/2013/04/02/emacs-async-file-copying-in-dired-using-rsync/),
I have demonstrated how to use rsync to replace the built-in Dired copy
function. Today, I have improved that function so that rsync window will dislay
progress separately from my currently working windows. Moreover, after finishing
copying, it will show the message and automatically close the progress window
after 5 seconds. Also, the rsync progress window can not be selected by call
other-window. As a result, the rsync feature now does not affect my current
work. I can continue what I'm currently doing while leaving rsync take care of my
files.

**Note**: it would not run on Windows computers.

# Implementation

This is the new code. Put it in your .emacs, replace with my old rsync function
if you have already use it.

**Update 8 Apr 2013**: I have improved the code a little bit so that later it
will be much easier for me to add new async function like moving,
compressing,... Also, this time, you can run multiple rsync process.

<!-- more -->

**tmtxt/dired-async function**: run the input command asynchronously

{% highlight cl %}
(defun tmtxt/dired-async (dired-async-command dired-async-command-name)
  "Execute the async shell command in dired.
dired-async-command: the command to execute
dired-async-command-name: just the name for the output buffer

Create a new window at the bottom, execute the dired-async-command and print
the output the that window. After finish execution, print the message to that
window and close it after 5s"
  (let ((dired-async-window-before-sync (selected-window))
		(dired-async-output-buffer
		 (concat "*" dired-async-command-name "*" " at " (current-time-string))))

	;; make a new window
	(tmtxt/dired-async-new-async-window)
	;; not allow popup
	(add-to-list 'same-window-buffer-names dired-async-output-buffer)
	;; run async command
	(async-shell-command dired-async-command dired-async-output-buffer)
	;; set event handler for the async process
	(set-process-sentinel (get-buffer-process dired-async-output-buffer)
						  'tmtxt/dired-async-process-handler)
	;; switch the the previous window
	(select-window dired-async-window-before-sync)))
{% endhighlight %}

**tmtxt/dired-async-new-async-window function**: create a new window for async
command to display output

{% highlight cl %}
(defun tmtxt/dired-async-new-async-window ()
  "Create a new window for displaying tmtxt/dired-async process and switch to that window"
  (let ((dired-async-window-height (- (window-total-height (frame-root-window)) 10)))
	(let ((dired-async-window
		   (split-window (frame-root-window) dired-async-window-height 'below)))
	  (select-window dired-async-window)
	  (set-window-parameter dired-async-window 'no-other-window t))))
{% endhighlight %}

**tmtxt/dired-async-process-handler function**: handle when process finish
execution, kill the buffer and the window that hold the process

{% highlight cl %}
(defun tmtxt/dired-async-process-handler (process event)
  "Handler for window that displays the async process.

Usage: After start an tmtxt/dired-async, call this function
 (set-process-sentinel process 'tmtxt/dired-async-process-handler)
process: the tmtxt/dired-async process

The function will print the message to notify user that the process is
completed and automatically kill the buffer and window that runs the
process."

  ;; check if the process status is exit, then kill the buffer and the window
  ;; that contain that process after 5 seconds (for the user to see the output)
  (when (equal (process-status process) 'exit)
	;; get the current async buffer and window
	(let ((current-async-buffer (process-buffer process)))
	  (let ((current-async-window (get-buffer-window current-async-buffer)))
		(print "Process completed.\nThe window will be closed automatically in 5s."
			   current-async-buffer)
		(set-window-point current-async-window
						  (buffer-size current-async-buffer))
		;; kill the buffer and window after 5 seconds
		(run-at-time "5 sec" nil 'kill-buffer current-async-buffer)
		(run-at-time "5 sec" nil 'delete-window current-async-window)))))
{% endhighlight %}

**tmtxt/dired-async-rsync function**: interactive function to execute rsync

{% highlight cl %}
(defun tmtxt/dired-async-rsync (dest)
  (interactive
   ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))

  (let ((files (dired-get-marked-files nil current-prefix-arg))
		dired-async-rsync-command)
	;; the rsync command
	(setq dired-async-rsync-command "rsync -arvz --progress ")
	;; add all selected file names as arguments to the rsync command
	(dolist (file files)
	  (setq dired-async-rsync-command
			(concat dired-async-rsync-command (shell-quote-argument file) " ")))
	;; append the destination to the rsync command
	(setq dired-async-rsync-command
		  (concat dired-async-rsync-command (shell-quote-argument dest)))
	;; execute the command asynchronously
	(tmtxt/dired-async dired-async-rsync-command "rsync")))
{% endhighlight %}

Finally, bind it to a key combination, in my case it it **C-c C-r**

{% highlight cl %}
(define-key dired-mode-map (kbd "C-c C-r") 'tmtxt/dired-async-rsync)
{% endhighlight %}

To use it, open up a dired window, select some file that you want to copy and
press the key combination **C-c C-r**, select the destination and press **RET**.
If you have the variable dired-dwim-target set to t, you can open 2
dired windows and this command will automatically choose the directory in the
other window as the default destination folder. The rsync progress window will
display in the bottom without affecting your current workspace. After finishing
copying, it will display a message and automatically close the rsync window
after 5 second.

In summary, we can use it in the same way that we used to use other dired copy
and move functions.

# Coming up next

I'm currently developing some functions that help emacs dired execute tasks
asynchonously (since dired is my default my management application). I'm
currently in those final weeks of the semester so I don't have much time. If you
want those features, wait for me to finish my semester in May.

# Screenshots

This is some screenshots from my Emacs.

![Rsync progress](/files/2013-04-06-emacs-async-file-copying-with-rsync-update-show-progress-and-auto-hide-after-finish/progress.png)  
The rsync process window when it is running.

![Rsync finish](/files/2013-04-06-emacs-async-file-copying-with-rsync-update-show-progress-and-auto-hide-after-finish/finish.png)  
The rsync window when it finish.
