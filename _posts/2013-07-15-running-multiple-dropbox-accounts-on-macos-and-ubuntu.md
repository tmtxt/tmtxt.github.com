---
layout: post
title: "Running multiple Dropbox accounts on MacOS and Ubuntu"
description: ""
category: misc
thumbnail: 
showtn: no
tags: [dropbox, macos, ubuntu]
---
{% include JB/setup %}

# Introduction

![cover](/files/2013-07-12-running-multiple-dropbox-accounts-on-macos-and-ubuntu/cover.png)

A free Dropbox account with 2GB of storage (can be up to 30GB for completing
some goals) seems to be not enough for our need. Also, for some people, they
want to separate Dropbox into 2 accounts, one personal use and one for business.
However, normally, we can use just one dropbox account at a time on our
computer. This article will demonstrates two methods to bypass Dropbox desktop
application and force it to run with multiple accounts concurrently.

# Run as another user

Have a look at the this post [Exploit Unix user account feature]({% post_url 2013-07-11-exploit-unix-user-account-feature%}),
especially the second part to know how this method works. The syntax of command
looks similar to this

{% highlight console %}
$ su username -c dropbox
{% endhighlight %}

This is tested successfully on MacOS, Linux Mint with Cinnamon desktop
environment. On Ubuntu Linux with Unity, KDE and XFCE desktop, I think there is
a problem with the
GUI, which prevents Dropbox from running using this method.

If you are on MacOS, you can refer to this article
[MacOS - Using multiple Google Drive accounts at the same time]({% post_url 2013-06-30-macos-using-multiple-google-drive-accounts-at-the-same-time %}).
You can follow the instruction there with a few changes to automate everything
on MacOS.

<!-- more -->

![MacOS](/files/2013-07-12-running-multiple-dropbox-accounts-on-macos-and-ubuntu/macos.png)

# Modify the HOME variable

Sometimes, the above method got problem with the permission since you have
to run it under another user permission. You may face the problem that you
dropbox cannot read or write to specific folder and you have to deal with
`chmod` command to fic the problem. The problem may occur with Proxifier
application (or some other proxy app), which prevents dropbox from connecting to
the internet. If you are not familiar with fixing the permission on Unix, you
can use the second solution presented here.

First, create a file name **start-dropbox-personal.sh** in your home directory.
This is the content of the file. Replace the **path/to/home/directory** and
**/path/to/dropbox/executable** with the real path of your home directory and
dropbox executable file on your computer.

{% highlight sh %}
#!/bin/bash
HOME=path/to/home/directory/.dropbox-personal /path/to/dropbox/executable start -i
{% endhighlight %}

Open terminal and change permission for that file and execute it

{% highlight console %}
$ chmod +x start-dropbox-personal.sh
$ ./start-dropbox-personal.sh
{% endhighlight %}

Now you can run the first dropbox account. Now repeat the above steps, just
change *personal* to something that you want and you are ready to run the second
dropbox account on your computer.

This method is tested to work successfully on MacOS and Ubuntu (Unity, XFCE,
KDE), Mint Linux (Cinnamon, Mate).

![Ubuntu](/files/2013-07-12-running-multiple-dropbox-accounts-on-macos-and-ubuntu/ubuntu.png)
