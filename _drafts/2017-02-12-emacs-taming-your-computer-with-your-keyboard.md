---
layout: post
title: "Emacs Taming your computer with your keyboard"
description: ""
categories: [emacs]
tags: []
thumbnail:
---

It has been 5 years since I started using Emacs, from when I was still a student. My Emacs configuration has evolved from a basic text editor for coding to a an eco-system for many tasks in my daily computer usage. Emacs is one of the very first applications that I have to open when I power up my laptop. I have been using it for many of my working tasks, from editing code files, managing git repositories, terminal emulating to even chating through IRC, composing this blog post and it really helps speed up my performance.

Writing about Emacs can be along story. However, in this post, I'm going to summarize some of the points which make Emacs a successor compare to its competitors and why I choose Emacs as my main text editor.

# Everything is controlled from the keyboard.

Yes, you didn't misread that. I said **everything**. Evreything in Emacs can be controlled from your keyboard. Emacs (and also Vim) is the text editor that came out from the terminal world. You can use Emacs from terminal just like you are using it within the GUI (except for colors). And yes, you know what I mean, no place for mouse in terminal environment.

Every key stroke that you type, every command that you need to run is defined as a function. You can easily bind/unbind any key combination to the commands that you frequently use. There is also a package that helps you measure the frequency of the commands that you usually use to help you decide which commands you need to give it a hot key.

Emacs keys system are based on modifier keys (Ctrl, Shift, Meta,...). Once you get yourself familiar with that, you can create many different key combinations that help you quickly activate the feature that you want without thinking too much about what to type. I already get used to those keys so there are many times in my daily working life, I can activate the feature that I want before my brain realizes that I will need to trigger that.

And hey, Emacs is equipped with a simple Vim mode by default. The community also made a package for a better Vim emulation experience called Evil and ported the corresponding Vim extensions to it. I don't want to start a flame war about text editor here but I found no reason to bother using Vim again after giving it a try. 😁

# A programming environment behind the shape of a text editor

Emacs is not really a text editor,. In fact, it's an interative programming environment comes with a built-in repl. Every config, every customization that you made to Emacs is written using Emacs Lisp (a dialect of Lisp). You have direct access to all Emacs functionalities, you can customize every aspect of Emacs without worrying whether it is exposed to any API or not, simply just by start writing some code.

Emacs also offers you a repl, where you can try your new feature or modify the existing features directly to see how it works without any worrying about breaking any of your current config. Just restart your editor and everything will be back to your previous state.

Thanks to Emacs, I got the chance to learn a new programming language, get started with functional programming style.

# Emacs as an operating system

Many people joke about Emacs as an operating system. That is, of course, impossible 😅. However, with its powerful extensiblity, we can tam Emacs to do many of our daily tasks that we have never thought of, and yes, faster.

We can use Emacs as a git client. 