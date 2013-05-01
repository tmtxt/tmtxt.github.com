---
layout: post
showtn: yes
title: "Dired as Default File Manager - Customize Faces"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, file manager, dired]
---
{% include JB/setup %}

> This post is the fifth part of the series
> [Dired as Default File Manager](/2013/04/24/dired-as-default-file-manager-1-introduction/).

Since version 24, Emacs offers a powerful tool to create and maintain custom
theme called customize-create-theme. It provides you an visual interface for
interacting with all those faces as well as previewing color in Emacs. You can
even copy from other theme and then customize it a bit to make it yours.

If you haven't known about Emacs 24 Color Theming yet, have a look at this post
[Color Theming in Emacs 24](/2013/03/31/color-theming-in-emacs-24/). This post
just lists some useful faces that you can customize for Dired.

<!-- more -->

**For Dired**

* **dired-directory**: Face used for subdirectories.
* **dired-header**: Face used for directory headers.
* **dired-mark**: Face used for dired marks.
* **dired-flagged**: Face used for files flagged for deletion.
* **dired-marked**: Face used for marked files.
* **dired-perm-write**: Face used to highlight permissions of group- and
world-writable files.
* **dired-symlink**: Face used for symbolic links.

**For Dired+**

* **diredp-date-time**: Face used for date and time in dired buffers.
* **diredp-deletion**: Face used for deletion flags (D) in dired buffers.
* **diredp-deletion-file-name**: Face used for names of deleted files in dired
buffers.
* **diredp-dir-heading**: Face used for directory headings in dired buffers.
* **diredp-file-name**: Face used for file names (without suffixes) in dired
buffers.
* **diredp-file-suffix**: Face used for file suffixes in dired buffers (file extension).

There are many more faces that you can explore in Emacs Dired.

**Previous part**:
[Dired as Default File Manager - Customize ls command](/2013/04/25/dired-as-default-file-manager-5-customize-ls-command/)  
**Next part**: 
[Dired as Default file manager - Dired Async](/2013/04/25/dired-as-default-file-manager-7-dired-async/)
