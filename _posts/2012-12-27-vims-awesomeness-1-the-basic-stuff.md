---
layout: post
title: "Vim’s Awesomeness #1: The basic stuff"
description: ""
category: blog
tags: [vim, text, editor, RMIT, tutorial]
---
{% include JB/setup %}

<p align="center"><img src="/files/2012-12-27-vims-awesomeness-1-the-basic-stuff/logo.png" /></p>
For those of you who are familiar with Unix-based OS, you must have know Vim text editor.  
For RMIT guys who have studied Programming Techniques, you must have been introduced to the famous (and somewhat intimidating) text editor called Vim. If you botherd to google around a bit, you will see lots of people praising it. Why?

# Because it’s fast

*Full version*: When you get used to it (which may look quite difficult at the beginning), you’ll find yourself extremely productive when using vim thanks to its unique charateristics:

### Fast startup, smooth performance and virtually no crashes:

Nothing annoys you more than wasting a whole minute waiting for the IDE to boot up just to edit a few lines of code. Vim is blazing fast even compared to other text editors (I’m looking at you, Emacs!).

### It’s a “modal” text editor,

which means it has different modes for specific purposes. In this case we have 3 modes: Insert, Normal and Visual. You only enter text normally in Insert mode. In the other 2 modes, numerous commands and actions can be done through quick keyboard shortcuts. Thanks to this, it’s completely keyboard-based and should of course make you look awesome when typing some cryptic stuff into a screen with no mouse cursor in front of your non-vim friends.

Since vim has a very steep learning curve, we will not post any tutorial on how to use it here. You can grab the official vim ebook here and dive in: <ftp://ftp.vim.org/pub/vim/doc/book/vimbook-OPL.pdf>

(Don’t worry if you think the book is too long. Just finishing chapter 6 will give you a pretty solid idea of using vim the right way.)

### Most importantly, it’s **completely customizable**.

By completely I mean it! You can do anything from remapping keys to writing auto-compile and testing scripts right inside vim. This gives way to a huge community writing countless awesome plugins for us to use for free. If there is some functionality you like about a certain IDE, there’s probably a vim plugin for it.

# Nope, still not convincing…

Ok, seeing is believing. Here’s what my vim looks like. By the way, this is GVIM running from RMIT Mekong server and yes, it’s using a graphical user interface, I’ll show you how to do this in another post.

<p align="center"><img src="/files/2012-12-27-vims-awesomeness-1-the-basic-stuff/gvim.png" /></p>

You can see some obvious stuff in the screenshot above:

* File explorer (which can create/rename/delete files and folders)
* Code (and even word) suggestion
* Source code explorer
* Code highlighting
* Syntax checking (YES, SYNTAX CHECKING!)

Of course there’s much more to it:

* Autoindenting
* Code snippets (click here if you’re wondering what it is)
* Git wrapper (graphical diff, etc.)

Again, Vim is good not just because of its plugins, but also thanks to its interesting way of editting. You can combine several commands to make it do a lot of cool stuff. For example: ”**gg**” (pressing g twice) moves the cursor to the beginning of a file. “=” indents a specified piece of code. “**G**” jumps to the last line of the file. What does “**gg=G**” do then? It indents the whole file! You see, the scope of “**=**” relies on the motion created by “**G**“. It’s almost like a mini programming language where “**=G**” means “go to the last line and indent everything on the way“. Pretty neat, eh?

I have heard some students complain about this “modal editor” approach, and they tend to be in insert mode all of the time. Well that is just **WRONG**! Normal mode should be your “default mode”. You should only enter Insert mode to enter some short “bursts” of code, then Esc the hell out of it. All the navigation and editing should be dealt with in the other 2 modes. The right Vim way is the one where you never use the arrow keys, I mean, never ever! Get used to **hjkl**, they’re your new friends. Once again, the Vimbook is a must for newbies, which explains all this stuff.

# Basic configurations

Let’s suppose you’re using a Linux distribution. Create a file named **.vimrc** and a folder named **.vim** in your home folder (that is **/home/username/**). The .vimrc will be where you place most of your configurations.

…

You know what? There are already plenty of good blog posts out there that show you how to do the basic .vimrc configs. I myself don’t think I can make a better post than this guy: <http://nvie.com/posts/how-i-boosted-my-vim/>

That post did mention Pathogen, which I will explain in the next section:

# Installing plugins

First install **Pathogen**. Download the [pathogen.vim](https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim) file then put it in **~/.vim/autoload** (you may have to create the folder **autoload** first). Then in your vimrc, put this line at the beginning of the file (if you have followed all the instructions in the nvie blog above then you don’t have to enter this line):

```
call pathogen#infect()
```

Now to install most plugins, you can just copy its folder into **~/.vim/bundle/** like this:

<p align="center"><img src="/files/2012-12-27-vims-awesomeness-1-the-basic-stuff/bundle.png" /><br/>
Each folder here is one plugin</p>

Considering that we’re IT people, I think by now you can go find these plugin and install them by yourselves with ease. Here is a list of my favorite plugins (download link? google is your friend):

* **NERDTree**: File explorer
* **NERDCommenter**: Quickly comment out blocks of code
* **Snipmate**: Pre-defined and custom code snippets
* **Supertab**: Code and word suggestion (it won’t show function signatures or things like that though)
* **Syntastic**: Syntax checking
* **Powerline**: Beautiful and informative status line
* **Tagbar**: Code explorer. It’s the left panel in my vim screenshot, which can show functions, global variables, and many more, depending on the programming language.

You can see most of these plugins in action here: <http://mirnazim.org/writings/vim-plugins-i-use/> . On that page you will notice that the author used git and github to manage his vim config. We will discuss that in another article. In the meantime you can check out my vim config on github: <https://github.com/nhanb/.vim>

#### Author
The author of this post is NhanB, who is a friend of mine. He wrote this post for RMIT VN IT Club.  
**Author**: Nhanb  
**Source**: <http://rmitc.org/2012/11/vims-awesomeness-part-1-basic-stuff/>