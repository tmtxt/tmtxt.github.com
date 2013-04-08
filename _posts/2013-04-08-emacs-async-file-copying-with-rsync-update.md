---
layout: post
title: "Emacs - Async File copying with Rsync - Update Apr 8 2013"
description: ""
category: Emacs
thumbnail: 
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
other-window. As a result, the rsync feature now does not affact my current
work. I can continue what I'm currently doing while leaving rsync take care of my
files.

**Note**: it would not run on Windows computers.

# Implementation

This is the new code. Put it in your .emacs, replace with my old rsync function
if you have already use it.

**Update 8 Apr 2013**: I have improved the code a little bit so that later it
will be much easier for me to add new async function like moving,
compressing,...

{% highlight cl %}
(defun tmtxt/dired-async-window-handler (process event)
	"Handler for window that displays the async process.

Usage: After start an tmtxt/dired-async, call this function
 (set-process-sentinel process 'tmtxt/dired-async-window-handler)
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
  
  ;; the rsync function
  (defun tmtxt/dired-async-rsync (dest)
	(interactive
	 ;; offer dwim target as the suggestion
	 (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))

	;; function body starts here
	(let								;init some variables
		;; store all selected files into "files" list
		((files (dired-get-marked-files nil current-prefix-arg))
		 ;; the current window for switch back after finish
		 (tmtxt/current-window-before-rsync (selected-window))
		 ;; the height of the rsync window
		 (tmtxt/new-rsync-window-height (- (window-height) 10))
		 ;; init those variables to nil
		 tmtxt/rsync-command tmtxt/new-rsync-window)
	  
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

	  
	  ;; split the window and set the newly created window to tmtxt/new-window
	  (setq tmtxt/new-rsync-window
			(split-window (frame-root-window) tmtxt/new-rsync-window-height 'below))
	  ;; switch to the new window
	  (select-window tmtxt/new-rsync-window)
	  ;; not allow the new window to be select by other-window
	  (set-window-parameter tmtxt/new-rsync-window 'no-other-window t)
	  ;; not popup new window for rsync
	  (add-to-list 'same-window-buffer-names "*rsync*")
	  
	  ;; run the async shell command
	  (async-shell-command tmtxt/rsync-command "*rsync*")

	  ;; add event handler for the rsync process
	  (set-process-sentinel (get-buffer-process (window-buffer tmtxt/new-rsync-window))
							'tmtxt/dired-async-window-handler)
	  
	  ;; finally, switch to the previous window
	  (select-window tmtxt/current-window-before-rsync)))
  ;; function finish here
  
  ;;; bind it to C-c C-r
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

# Screenshots

This is some screenshots from my Emacs.

![Rsync progress](/files/2013-04-06-emacs-async-file-copying-with-rsync-update-show-progress-and-auto-hide-after-finish/progress.png)  
The rsync process window when it is running.

![Rsync finish](/files/2013-04-06-emacs-async-file-copying-with-rsync-update-show-progress-and-auto-hide-after-finish/finish.png)  
The rsync window when it finish.

# Coming up next

I'm currently developing some functions that help emacs dired execute tasks
asynchonously (since dired is my default my management application). I'm
currently in those final weeks of the semester so I don't have much tim. If you
want those features, wait for me to finish my semester n May.
