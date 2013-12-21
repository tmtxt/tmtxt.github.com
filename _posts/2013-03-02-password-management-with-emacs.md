---
layout: post
showtn: yes
title: "Password Management with Emacs"
description: ""
category: Emacs
thumbnail: /files/thumbnails/emacs.png
tags: [emacs, password management]
---
{% include JB/setup %}

# Intro

It's a great idea to encrypt your identity such as username and password for the
services that you use. Since version 23, Emacs has been integrated with a
package called [EasyPG](http://epg.sourceforge.jp/) to help us with those task.
It's an interface to
[GnuPG](http://www.gnupg.org/) so make sure that you have GnuPG installed on
your computer. If you're on MacOS, you can easily install it from Macports by
executing the command **sudo port install gnupg**.

# Usage

First, make sure that you have GnuPG installed on your computer by typing
**which gpg** in terminal. On MacOS, you can install it from Macports. After
finishing installing gnupg, add this to your .emacs

{% highlight cl %}
(when (file-executable-p "/opt/local/bin/macports/bin/gpg")
	(setq epg-gpg-program "/opt/local/bin/macports/bin/gpg"))
{% endhighlight %}

<!-- more -->

To use EasyPG, simply just add **.gpg** to anyfile that you want to encrypt. For
example, I wish to use markdown mode to organize my password file, visit that
file by typing C-x C-f and then type in the file name **password.md.gpg**. (Make
sure you have markdown mode installed in your emacs). Emacs will now open that
file in markdown mode like any other .md files. It's really simple and can not
be easier. Everything you need to do now is just store any data that you want in
that file. When finish, save that file by typing C-x C-s. Emacs will ask for
encryption type that you want to use. Choose OK to use the default symmetric
encryption. Type in the passphrase for your password file and remember that
passphrase. It's will be the key for you to open the file again.

# Always use symmetric encryption

To prevent EPG from prompting for a key every time you save a file, put the following at the top of your file:

{% highlight sh %}
    -*- epa-file-encrypt-to: ("your@email.address") -*-
{% endhighlight %}

EPA will prompt for the key only the first time you save the file, assuming you have the email address you specified in your keyring.

# Reference

[Emacs-fu - Keeping your secrets secret](http://emacs-fu.blogspot.com/2011/02/keeping-your-secrets-secret.html)  
[Easy PG on Emacswiki](http://emacswiki.org/emacs/EasyPG)
