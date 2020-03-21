---
layout: post
title: "A nice development experience on Windows"
description: ""
categories: [misc]
tags: []
thumbnail:
---

> Yes, \*nix users can still have a nice development experience on Windows

So... I built a new PC at home with the release of AMD Ryzen 3000 CPU generation last year. I
initially built it for my gaming purpose and running some legacy projects written in `.Net 4.7`.
However, I quickly found how convenient it is to have another home working computer so I can leave my
Macbook at the office.

![PC1](/files/2020-03-15-a-nice-development-experience-on-windows/pc1.jpg)

However, for a guy haven't used Windows for nearly 10 years and living deeply inside the \*nix
ecosystem like me, it was really a big challenge. As a result, I started with installing a
Linux distro (Ubuntu/Kubuntu) on my PC (because at that time, Windows was still weird as f**k for
me). I immediately realized that if you haven't used MacOS, you can stick with those Linux desktop
environments but once you have, you will hardly be satisfied with any Linux desktop. Yes, Linux is
still my favorite choice for server OS, but not for desktop PC.

I started taking a serious look at using Windows as my main OS at home again. Here is my story in
taming the Windows PC beast for daily development life.

# First thing first: A package manager

For the guys coming from Linux/Unix world, a `good` package manager is an essential part of their
daily lives. Being familiar with the old versions of Windows (Windows XP, 10 year ago), I have never
imagined that there are solution to package manager out there for Windows (actually, I didn't know
at that time). There are currently 2 popular solution to Windows package manager: `chocolatey` and
`scoop`. Each is designed in a different way and whether to choose `scoop` or `chocolatey` is really
controversal. For me, the best solution is to use both of them. Each has their own use case

- `scoop` 
