---
layout: post
title: "Aria2 as Default Download Manager on Unix/Linux"
description: ""
category: misc
thumbnail: 
showtn: no
tags: [aria2]
---
{% include JB/setup %}

> aria2 is a lightweight multi-protocol & multi-source command-line download
> utility. It supports HTTP/HTTPS, FTP, BitTorrent and Metalink. aria2 can be
> manipulated via built-in JSON-RPC and XML-RPC interfaces.

I'm currently using a Mac. There are many good download manager for MacOS out
there, for example Folx, Speed Download, JDownloader, iGetter, DownThemAll,
uTorrent, Transmission,...
However, most
of them have drawbacks. Speed Downloader and iGetter are not free, you
have to pay in order to use it and you cannot download torrent with them. Folx
offers a free version but the
functionality is limited. Also, Folx 3 has removed the ability to download
torrent file. DownThemAll requires Firefox to be opened. uTorrent and
Transmission are just torrent specialized. But the most annoying problem for me
is that they are not light-weight applications. I want some simple, free
but light-weight app that can support multiple protocols from http, ftp to
torrent.

Finally, I found **aria2**, a CLI download manager that can satisfy all
my demands. When disk cache is off, the physical memory usage is typically 4MiB
(normal HTTP/FTP downloads) to 9MiB (BitTorrent downloads). CPU usage in
BitTorrent with download speed of 2.8MiB/sec is around 6%.

However, sometimes the
command line brings more power than necessary. I
also need a simple GUI for working with the app more easily.
[Webui-aria2](https://github.com/ziahamza/webui-aria2) is the solution. One of
the great feature of **webui-aria2** is that it's a web-based UI. I can use the
browser (Conkeror or Chrome with Vimium) to interact with it (add, pause, remove
downloads) using the keyboard very quickly.

Here are the steps on how to setup aria2 client on a Unix/Linux system.

**Note**: If you'are using Linux, there are an application that is the GUI
wrapper for aria2 called [uGet](http://ugetdm.com/).

<!-- more -->

# Install aria2

On Mac OS X, you can install **aria2** using Macports or Fink. In case you are
using Macports, execute this command to install

{% highlight console %}
$ sudo port install aria2
{% endhighlight %}

On Debian and Ubuntu, install it using **apt-get**

{% highlight console %}
$ apt-get install aria2
{% endhighlight %}

On Fedora, install it using **yum**

{% highlight console %}
$ yum install aria2
{% endhighlight %}

For other platforms, have a look at
[http://sourceforge.net/apps/trac/aria2/wiki/Download](http://sourceforge.net/apps/trac/aria2/wiki/Download).

When you have finished installing **aria2**, use this command to check

{% highlight console %}
$ which aria2c
{% endhighlight %}

# Install webui-aria2

Actually, this is just a static web page. Everything you need to do is to clone
the repository on github to your computer using **git**.

{% highlight console %}
$ git clone https://github.com/ziahamza/webui-aria2.git
{% endhighlight %}

If you don't have git installed, just go to the repo page on github at
[https://github.com/ziahamza/webui-aria2](https://github.com/ziahamza/webui-aria2) and select the button
**Download Zip**. Download it to your computer and extract it.

# Start using aria2

You need to start **aria2** from Terminal. Open Terminal and cd to the directory
that you want **aria2** to save the downloaded files in.

{% highlight console %}
$ cd ~/mydownload
{% endhighlight %}

Start **aria2** using the following command

{% highlight console %}
$ touch session.txt
$ aria2c --enable-rpc --rpc-listen-all --save-session=session.txt -isession.txt -x16 -k1M
{% endhighlight %}

Now **aria2** should be running and listening on port 6800. You should see
something like this in Terminal

{% highlight console %}
11/12 11:17:09 [NOTICE] IPv4 RPC: listening on TCP port 6800
11/12 11:17:09 [NOTICE] IPv6 RPC: listening on TCP port 6800
{% endhighlight %}

Next, use your web browser to open the file **index.html** inside the
webui-aria2 folder that you have just downloaded. The web page will look like
this picture, otherwise there maybe some problems with your aria2 client.

![Webui-Aria2]({{BASE_PATH}}/files/2013-11-12-aria2-as-default-download-manager/aria2.png)


From there, you can start using **aria2**. Take some minutes to familiarize
yourself to its UI.

If you want to stop aria, just go back to Terminal and press Ctrl-C. All the
error/unfinished downloads will be saved into **session.txt** file. Next time
when you want to use aria2 again, just start with the same command.

# Automate everything.

If you are tired of typing the long command again and again, simply assign an
alias for it in your shell rc file (.bashrc, .zshrc,...).

{% highlight sh %}
alias myaria2='aria2c --enable-rpc --rpc-listen-all --save-session=session.txt -isession.txt -x16 -k1M'
{% endhighlight %}

This solution still requires you to cd to your download folder manually and
execute the *touch* command for the first time but it gives you the flexibility
to choose your download folder and organize multiple download locations. If you
just want to have only one download folder, create a shell script to do all the
job. Create a file name **start-aria2.sh** in your home directory with the content
like this.

{% highlight sh %}
cd ~/down
touch session.txt
aria2c --enable-rpc --rpc-listen-all --save-session=session.txt -isession.txt -x16 -k1M
{% endhighlight %}

Next, change the permission to make it executable by the command

{% highlight console %}
$ chmod +x start-aria2.sh
{% endhighlight %}

Now, everytime you want to run **aria2**, just double click on that file or
start it via the command line

{% highlight console %}
$ ~/start-aria2.sh
{% endhighlight %}

Finally, bookmark the webui-aria2 index.html file in your browser for quick
access.
