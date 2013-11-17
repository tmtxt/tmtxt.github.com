---
layout: post
title: "MacOS - Copy folder path using keyboard"
description: ""
category: misc
thumbnail: 
showtn: no
tags: [macos]
---
{% include JB/setup %}

Copying folder path is a regular task that I use almost every day. It would be
very useful if I can quickly copy the path that I'm currently working in Finder
or Terminal and then switch to other application to continue working on that
folder. Finder does not have any built-in feature for copying the current path
using the keyboard. However, we can use a workaround with help from Apple
Automator.

# Finder

To copy the current path in Finder, open Automator and create a new Service.
Config this service to receive **no input** in **Finder** application. After
that, add **Run AppleScript** action to the work flow. Replace
`(* Your script goes here *)` with this

{% highlight applescript %}
try
    tell application "Finder" to set the clipboard to POSIX path of (target of window 1 as alias)
on error
    beep
end try
{% endhighlight %}

If you want it to copy as quoted, you can change `POSIX path` to
`quoted form of POSIX path`.

<center>
<img src="/files/2013-09-15-macos-copy-folder-path/automator.png" />
</center>

Save the service with a name that you want. Remember that name so we
can use it later to bind a shortcut key for it.

<!-- more -->

Next, open **System Preferences**, **Keyboard**, and select tab
**Keyboard Shortcuts**. Under **Application Shortcuts** section, click **+**
button to add a new shortcut. Select **Finder** from Application, type in the
name of the service you have created before and set a keystroke for it in
**Keyboard Shortcut** textbox.

<center>
<img src="/files/2013-09-15-macos-copy-folder-path/syspref.png" />
</center>

Now at any folder in **Finder**, type press the shortcut before and the full
path is copied into clipboard.

<center>
<img src="/files/2013-09-15-macos-copy-folder-path/finder.png" />
</center>

After copying the path, you can go to your browser, select upload file, or open
any other application and select open file,... When a file browser appear, press
**cmd + shift + G** and paste the path into it and you're taken to that
directory immediately without remembering exactly where it is. Very useful and
save time.

# Terminal

In most Unix-based OS, there usually a command called **pwd**, which means print
working directory. In MacOS, you can use that command in combination with pbcopy
to copy the output of pwd to clipboard.

{% highlight console %}
$ pwd | pbcopy
{% endhighlight %}

# Emacs

This section is not only for MacOS. It's for all people who are using Emacs as
their main file manager. In an Emacs Dired mode, pressing **w** on any file will
copy the name of that file. Pressing **0 w** will copy the full path of that
file into clipboard.
