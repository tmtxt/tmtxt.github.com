---
layout: post
showtn: 
title: "MacPorts - The MacOS package manager"
description: ""
category: Misc
thumbnail: /files/2013-01-01-macports---the-macos-package-manager/macports.png
tags: [macports, package manager, macos]
---
{% include JB/setup %}

# Introduction

For Ubuntu Linux (and Mint), we have a powerful package manager called
**apt-get**, which makes
the installation, uninstallation and management of packages much easier. Sadly,
for MacOS users, we don't have that powerful tool. The appearance of
**MacPorts**, an open source package manager for MacOS, has changed the way we
install and manage our packages. With the help of **MacPorts**, MacOS users can
now quickly install, keep track of the changes, update and maintain many
open-source
applications. You can even export all your installed packages and then
let **MacPorts** automatically reinstall them for you when you have your
computer reinstalled or when you migrate to another system.

# Installation

The easiest way to install **MacPorts** is to download the .pkg installer
[here](http://www.macports.org/install.php). Select the installer corresponding
to your current OS. Alternatively, you can build it from source. For more
detail, please visit [this link](http://www.macports.org/install.php).

After finishing the installation, you should consider updating **MacPorts**.
Fortunately, **MacPorts** has the built-in feature for selfupdating. Everything you
need to do is to execute this command

<!-- more -->

{% highlight sh %}
$ sudo port -v selfupdate
{% endhighlight %}

Now you can enjoy one of the most powerful package manager on MacOS. :)

# Basic usage

To list all available ports, type this command (it takes a few minutes to load)

{% highlight sh %}
$ port list all
{% endhighlight %}

To search for a port, type:

{% highlight sh %}
$ port search portname
{% endhighlight %}

To get info of a port, type:

{% highlight sh %}
$ port info portname
{% endhighlight %}

To install a port, execute this command. The installed apps are usually put in
/Application/MacPorts

{% highlight sh %}
$ sudo port install portname
{% endhighlight %}

To delete an installed package, type

{% highlight sh %}
$ sudo port uninstall portname
{% endhighlight %}

To upgrade an installed port, type

{% highlight sh %}
$ sudo port upgrade portname
{% endhighlight %}

To upgrade all outdated ports, type:

{% highlight sh %}
$ sudo port upgrade outdated
{% endhighlight %}

To list all installed packages, type:

{% highlight sh %}
$ sudo port list installed
{% endhighlight %}

So those are some basic commands for MacPorts. Actually, the most inportant ones
are
**install**, **uninstall** and **upgrade**. Now you can use **MacPorts** to
install many open source application like VLC, Emacs, Handbrake, iTerm,
VirtualBox, Qt,...

# Migration

Since **Macports** is a package manager, it has the built-in support to make the
migration process
become easier. You don't have to remember which packages you've installed.
Everything you need is to export the installed packages list and bring it to the
destination system where you want them to be reinstalled.

To export the list of installed ports, type:

{% highlight sh %}
$ port -qv installed > myports.txt
{% endhighlight %}

The command above will write all the ports that you've have installed to
myports.txt.

On the destination computer, you need to uninstall all packages to avoid
conflict.

{% highlight sh %}
$ sudo port -f uninstall installed
{% endhighlight %}

and then clean any partially-completed builds

{% highlight sh %}
$ sudo port clean all
{% endhighlight %}

After that run this command for **MacPorts** to automatically reinstalled all
packages

{% highlight sh %}
$ curl -O https://svn.macports.org/repository/macports/contrib/restore_ports/restore_ports.tcl
$ chmod +x restore_ports.tcl
$ sudo ./restore_ports.tcl myports.txt
{% endhighlight %}

It's possible to have conflict during the installation process. If it happens,
just delete the conflicting ports from myports.txt and run the script again. You
may have to do this several times.

-----

**MacPorts homepage**: <http://www.macports.org/>  
**Available Ports**: <http://www.macports.org/ports.php>
