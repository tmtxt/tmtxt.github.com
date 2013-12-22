---
layout: post
title: "Emacs - Find and Replace String regex in multiple files"
description: ""
category: emacs
tags: [emacs]
---
{% include JB/setup %}

When working with multiple files, especially for a set of files that
contain the same header, we occasionally want to edit the header. This leads to
the need to update all other files as well. Fortunately, Emacs has the built in
command for interactively find and replace a String pattern in multiple files.
Here are the steps on how to quickly find and replace text in multiple files.

* Open the directory that contains all the files web need to find and replace in
  dired mode.
* Mark the files you want to find the text. Hit `t` (`dired-toggle-marks`) to
  select all files in the current folder
* Type `Q` (`dired-do-query-replace-regexp`), type in the pattern that you
  want Emacs to find and the replace string. Hit `RET`.
* For each occurrence, type `y` to replace, `n` to skip. Type `Ctrl+g` to abort the
  whole operation.
* Type `!` to replace all occurrences in current file without asking, `N` to
  skip all possible replacement for rest of the current file.
* To do the replacement on all files without further asking, type `Y`.
* Call ibuffer to list all opened files.
* Type `* u` to mark all unsaved files, type `S` to save all marked files, type
  `D` to close them all.

<!-- more -->

**Reference**: [ergoemacs](http://ergoemacs.org/emacs/find_replace_inter.html)
