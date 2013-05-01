---
layout: post
showtn: yes
title: "Cross Platforms Synchronization Solution"
description: ""
category: Misc
thumbnail: 
tags: [synchronization]
---
{% include JB/setup %}

I'm a technology fanboy, or in other word, a geek. I'm really interested in
computers, smartphones, high tech devices,... I have used Windows, MacOS,
Ubuntu, iOS, Android, Blackberry, Symbian,... I want to keep them all in sync so
that I can easily manage and access my data on any devices. Here are some of my
currently used and known solutions for keeping them in sync.

<!-- more -->

# Contacts and Calendar sync

Sync solution for each platform
* Android: CardDAV/CalDAV, sync with iCloud using Smoothsync for cloud
* Gmail: CardDAV/CalDAV
* MacOS: CardDAV/CalDAV, Microsoft Exchange, iCloud
* iOS (in my case iPhone 3G iOS 4.2.1 and iPhone 3GS iOS 6.1): Microsoft
Exchange, iCloud, sync with Mac/PC using iTunes
* WindowsPhone: Microsoft Exchange, CardDAV/CalDAV (available soon)
* Windows: Microsoft Exchange

We can also use a combination of many methods. For example, in my case, I own
one Macbook, one Android phone, one iOS phone, I want my contact list and
calendar, events,... stay them same on those 3 devices so that when I use my
computer, I can organize my working diary, events and then automatically, they
are pushed into my phone and notify me when I have some task to do no matter
what device I'm currently using. I can setup my Gmail as the center and then
sync my Mac, Android with Gmail using CardDAV/CalDAV protocol and sync my iPhone
with Gmail using Exchange. Another solution is to let iCloud be the center for
everything, sync all Android, Mac and iPhone to my iCLoud account.

The list is not the full one, just the ones that I know for sure. There are
various methods. All of them requires us to spend time before using, which can
be very annoying for lazy people :LOL:. However, once we've done the config,
everything will become simple.

# Media sync

## iOS and MacOS/Windows

**iTunes** is the best answer, of course. All of them are Apple's products and I
  encounter no difficulty when syncing them. It's even more convience when you
  use other Apple's products. For instance, with iPhoto (MacOS only) as the photo manage app,
  you can easily sync all your albums, events as well as people/faces in those
  images.

Another option is to use the Apple's cloud service **iCloud**. There is nothing
much different from iTunes but it allows you to sync over the internet.

## Android and MacOS

[doubleTwist](http://www.doubletwist.com/) performs very well. I can easily
import my music library from iTunes or add them from my computer and then sync
them to my Android smartphone (require it to be in USB Storage mode). Moreover,
it also automatically convert my lossless file to lower bit rate to fit my
phone.  
The free version of doubleTwist allows you to tether sync Mac and Android over
USB cable while the premium one adds many more function like Over the air
Synchronization.

## Over the Cloud Syncing

### Dropbox

Dropbox offers applications on many platforms so that you can easily manage and
sync your files. It also provides Camera Upload feature, which lets your phone
upload photo right after you shoot so that you will never lose any of your
valuable picture even if you lost or break your phone.

### SugarSync

SugarSync offers you a free 5GB account at register time but you can maximize
the it to 32GB. It's also a great service for syncing music over the cloud and
listen to them on any of your devices.

### iCloud

This is for iOS users only. You can refer back to **iOS and MacOS/Windows**
section above.

# Summary

Of course there are still many other methods. Here is just what I know from my
experience. Synchronization can be a little complicated at config phase but it
helps me a lot in working with multiple devices.
