---
layout: post
title: "Fix error: Ubuntu forced to run in low graphic mode in VirtualBox 4.2.12"
description: ""
category: Misc
thumbnail: 
showtn: yes
tags: [ubuntu, virtualbox]
---
{% include JB/setup %}

![Error](/files/2013-07-02-how-to-fix-ubuntu-forced-to-run-in-low-graphic-mode-in-virtualbox/QaP9d.png)

There is an annoying bug in VirtualBox 4.2.12 that makes the virtual Ubuntu
machine cannot display properly. After finishing the installation and reboot, it
keeps telling me that Ubuntu have to run in low graphic mode and prevents me
from loggin in Ubuntu desktop.

<!-- more -->

To fix this, there are two solutions. The first one is that you simply switch
back to VirtualBox 4.2.10 and the problem will be gone. However, if you still
want to experience the new version, you need to do some shell commands. After
the error message appears, select `Exit to console login` or
`Run in low-graphics mode for just one session`. In my case, both options take
me to the shell login interface. Enter you username and password to login and
execute these two commands for installing the patch.

{% highlight console %}
$ sudo apt-get install fglrx
$ sudo reboot
{% endhighlight %}

The virtual machine will now reboot and the problem's gone.
