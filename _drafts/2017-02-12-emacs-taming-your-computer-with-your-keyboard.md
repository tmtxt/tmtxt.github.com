---
layout: post
title: "Emacs Taming your computer with your keyboard"
description: ""
categories: [emacs]
tags: []
thumbnail:
---

It has been 5 years since I started using Emacs, from when I was still a student. My Emacs
configuration has evolved from a basic text editor for coding to an ecosystem for many tasks in
my daily computer usage. Emacs is one of the very first applications that I have to open when I
power up my laptop. I have been using it for many of my working tasks, from editing code files,
managing git repositories, terminal emulating to even chatting through IRC, composing this blog post
and it really helps speed up my performance.

Writing about Emacs can be along story. However, in this post, I'm going to summarize some of the
points which make Emacs a successor compare to its competitors and why I choose Emacs as my main
text editor.

# Everything is controlled from the keyboard.

One of the greatest thing about Emacs is that **everything** can be controlled from your keyboard.
Emacs (and also Vim) is the text editor that came out from the terminal world and is designed for
using without mouse interaction.

Every key stroke that you type, every command that you need to run is defined as a function. You can
easily bind/unbind any key combination to the commands that you frequently use. There is also a
package that helps you measure the frequency of the commands that you usually use to help you decide
which commands you need to give it a hot key.

Emacs keys system are based on modifier keys (Ctrl, Shift, Meta,...). Once you get yourself familiar
with that, you can create many different key combinations that help you quickly activate the feature
that you want without thinking too much about what to type. I already get used to those keys so
there are many times in my daily working life, I can activate the feature that I want before my
brain realizes that I will need to trigger that.

And hey, Emacs is equipped with a simple Vim mode by default. The community also made a package for
a better Vim emulation experience called Evil and ported the corresponding Vim extensions to it. I
don't want to start a flame war about text editor here but I found no reason to bother using Vim
again after giving it a try.

# A programming environment under the shape of a text editor

Emacs is not really a text editor. In fact, it's an interactive programming environment comes with a
built-in repl. Every configuration, every customization that you made to Emacs is written using
Emacs Lisp (a dialect of Lisp). You have direct access to all Emacs low-level functions, you can
customize every aspect of Emacs without worrying whether it is exposed to any API or not, simply
just by start writing some code.

Emacs also offers you a repl, where you can try your new feature or modify the existing features
directly to see how it works without any worrying about breaking any of your current configuration.
Just restart your editor and everything will be back to your previous state.

Thanks to Emacs, I got the chance to learn a new programming language, get used to functional
programming style and play with all those macros to customize the language in many ways that I want.

# Emacs as an operating system

Many people joke about Emacs as an operating system. That is, of course, impossible. However, with
its powerful extensibility, we can tam Emacs to do many of our daily tasks that we have never
thought of, and yes, faster.

We can use Emacs as a git client.

# Final thought

For me, Emacs is not just a text editor, it's more like my working style. I love the idea of
controlling everything by keyboard through a text-based UI. I have never thought of that before that
I can do
