---
layout: post
title: "Mac OS - Show hidden files in Finder"
description: ""
category: misc

showtn: no
tags: [macos, finder]
---
{% include JB/setup %}

edit /Volumes/tmtxt/Library/Preferences/com.apple.finder.plist

find key AppleShowAllFiles, change to YES, relaunch Finder
