---
layout: post
showtn: yes
title: "Conkeror - Getting Started - Part 1"
description: ""
category: conkeror
tags: [conkeror, emacs, web browser]
---
{% include JB/setup %}

> **Note**: before you read, I just want to remind you that Conkeror is not for newbie or non-IT users. It might take you a lot of time to get used to it. So if you are not programmers, please close this article and go back. But once you got used to it, you will feel extremely comfortable and get your work done faster.  
> Again, if you are not really interested in, read this article [Conkeror and how it changed the way I surf the web](/2012/12/24/conkeror-and-how-it-changed-the-way-i-surf-the-web) to be convinced or just leave this page. Don't just try and give up early and blame me on wasting your time :LOL:.  
> If you interested in, let's go!

<!-- more -->

**__Conkeror Installation __**

Of course, the very first thing when we want to use a software is to install it.
Conkeror is based on XULRunner, which is the core of Firefox, too. There are two
ways that we can run Conkeror on our computer. First, install it using
XULRunner. The other way is to install it based on Firefox. Conkeror is just
like a GUI, and it run based on the XULRunner or Firefox. So if we want to use
Conkeror, we need 2 things, Conkeror and either XULRunner or Firefox.

I recommend you to install it based on Firefox.

	Update: the downloading system seems to be fixed in Firefox 21, so I now recommend you to install it base on Firefox 21. Don't care about the note below.

> Note: There is currently a problem with the downloading system if you are using Firefox/XULRunner version 14 or above (now the XULRunner and Firefox have the same version number), so if you don't mind, just install the lastest Firefox/XULRunner version and enjoy Conkeror. Otherwise, I advise you to install Firefox/XULRunner 12. Don't worry too much since Firefox/XULRunner releases the new version too fast so there are little changes in each version ;) Just use the 12 version, it is adequate, and wait until someone fix it and contribute the Conkeror or you can be the hero ;)

## Preparation:  

Here is the list of things you need to prepare in order to run Conkeror

**Firefox** again I recommend Firefox version 21 ;)

**Git** you need git to obtain conkeror source code. Please, forget the SVN. I hate it anyway :LOL:

## Installation:  

**Note**: For Mac OS user, there is an easier way to install conkeror, see the
  section below.

First, you need to obtain Conkeror from the repo, open up Terminal and type this at the prompt. Remember you need to have git installed before.

{% highlight sh %}
$ git clone git://repo.or.cz/conkeror.git
{% endhighlight %}

Alternatively, you can download a snapshot archive from this link: <http://repo.or.cz/w/conkeror.git?a=snapshot;h=master;sf=tgz> and then extract it

I recommend you to use git to clone that repo cause it's easy to update conkeror.

After cloning the repo or extracting the archive, open up the directory where you've just cloned/extracted conkeror, you will see a file named application.ini.

Open up terminal again, type this at the prompt

{% highlight sh %}
$ firefox --app /path/to/application.ini
{% endhighlight %}

In that command above, **firefox** is the command to run your Firefox browser. On windows, it's usually the path to firefox.exe file. On MacOS, open finder and browse to your Firefox.app location, right click on it -> Show Package Contents, continue to go to folder Contents->MacOS, there is a file named "firefox" there. Drag and drop it into your terminal, and continue to type the rest "**--app /path/to/application.ini**". On ubuntu, if you have firefox loaded into your $PATH, simply type **firefox**.

**/path/to/application.ini** to the path to the application.ini file I mentioned before.

Hit Return (Enter on Windows) when you're done and see the magic ;)

Conkeror should appear and there should be something looks like this

![Conkeror Main Window](/files/2012-12-24-conkeror-getting-started-part-1/conkeror-main.png) 

The installation steps look complicated when you first time see it. But in fact it's just one command: firefox --app /path/to/application.ini. Call firefox and pass the application.ini path as the argument and you're ready to run Conkeror

### For Mac OS users

There is another installation method for Mac OS user, which is much easier.
You will need **Firefox.app** installed in /Applications, **gcc** and of
course **git**. First clone this
[repo](https://github.com/smazga/conkeror_mac_bundler) or download the [archive](https://github.com/smazga/conkeror_mac_bundler/archive/master.zip).

{% highlight sh %}
$ git clone git@github.com:smazga/conkeror_mac_bundler.git
{% endhighlight %}

Cd to the repo directory you've just clone <span>(conkeror_mac_bundler)</span>,
run

{% highlight sh %}
$ make install
{% endhighlight %}

Wait until the installation process finish, there will be an Conkeror.app
application in the directory. Open it and Conkeror is ready to run.

## Automate it:

Instead of typing that command everytime you want to launch conkeror, simply
just give it an alias in your shell config. For the other OS, if you know anyway to automate it, you're are
welcome to contibute.

Now Conkeror is ready to run on your computer. If you want, you can read the
tutorial that you see or you can close it now and wait for my next post :LOL:

-----

Conkeror Homepage: <http://conkeror.org/>  
My conkeror on github: <https://github.com/tmtxt/conkerorrc>  
Follow me and we can exchange the experience.

Next: **[Conkeror - Getting Started - Part 2](/2012/12/25/conkeror---getting-started---part-2/)**
