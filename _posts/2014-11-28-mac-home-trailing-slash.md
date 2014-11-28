---
layout: post
title: "MacOS - Fix trailing slash in $HOME variable"
description: ""
categories: [mac]
tags: []
---
{% include JB/setup %}

On my Mac, I always have 2 different partitions, one for installing the system
and the other one for my home directory. Today, I've just re-installed the whole
system and of course I have to specify the home path after the installation.
However, after changing the home directory, many of my applications cannot work
properly. I have checked and recognize that the `$HOME` variable in the shell
now has a trailing slash (`/Volumes/tmtxt/` instead of `/Volumes/tmtxt`).

That was so annoying and took me hours to investigate. Finally I found the
problem. It comes from one of my carelessness when I change the home directory
location.

Yes, I typed the path manually... How stupid I was. Why not just use the
`Choose` button right next there :LOL:

![Alt Text](/files/2014-11-28-mac-home-trailing-slash/user.png )

Just change it to another folder, restart the computer and change back to the
right home folder. Everything will be fixed automatically.

<!-- more -->
