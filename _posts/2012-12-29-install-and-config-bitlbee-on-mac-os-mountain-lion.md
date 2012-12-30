---
layout: post
title: "Install - Config Bitlbee on Mac Mountain Lion"
description: ""
category: 
tags: [bitlbee, mac, mountain-lion, tutorial]
---
{% include JB/setup %}

# Introduction to Bitlbee

Quote from Bitlbee homepage

> BitlBee brings IM (instant messaging) to IRC clients. It's a great solution
> for people who have an IRC client running all the time and don't want to run
> an additional MSN/AIM/whatever client.  
> BitlBee currently supports the following IM networks/protocols: XMPP/Jabber
> (including Google Talk), MSN Messenger, Yahoo! Messenger, AIM and ICQ, and the
> Twitter microblogging network (plus all other Twitter API compatible services
> like identi.ca and status.net).

In short, Bitlbee is like an IRC server where you can connect to and then use it
with your IRC clients to connect to other chat services like Yahoo, FB, Gtalk,...

**Bitlbee Homepage**: <http://www.bitlbee.org/>

# Installation

First you need to install [MacPort](http://www.macports.org/), a package manager
for Mac OS. Simply download
the installation package [here](http://www.macports.org/install.php) and install
it into your computer. 

After finishing MacPort installation, open Terminal and type in this command

{% highlight sh %}
sudo port install bitlbee
{% endhighlight %}

Provide it your root password and wait until MacPort successfully installed
bitlbee for you.

If you run **bitlbee** immediately, you will receive a message that btlbee cannot
read the config. We need some extra steps.

# Configuration

First we need to have the bitlbee.conf file to store all the config of bitlbee.
Open up terminal again, cd to the this directory

{% highlight sh %}
cd /opt/local/etc/bitlbee
{% endhighlight %}

Type **ls** and press **return**,
you will see a file named bitlbee.conf.sample here. This is a sample of the
config file. What we need to do now is to rename it to remove the .sample
trailing. Issue this command to rename it

{% highlight sh %}
sudo mv bitlbee.conf.sample bitlbee.conf
{% endhighlight %}

Type **ls** again and hit **return** to see if the file name is successfully changed.

Now you can run **bitlbee**, but wait! If you connect to bitlbee server and register an
account there, it will return an error that the program does not have enough
permission. We need some more configuration to make it run properly.

Before doing the next steps, you need to determine your username and group. Do
not close terminal. Open **System Preference**, click on **User & Group**, click on the lock.

<p align="center"><img
src="/files/2012-12-29-install-and-config-bitlbee-on-mac-os-mountain-lion/lock.png"></p>

You need to input your password. After that, right click on your account on the
left sidebar and select **Advanced Option**.

<p align="center"><img
src="/files/2012-12-29-install-and-config-bitlbee-on-mac-os-mountain-lion/advanced.png"></p>

Look at the textbox Group and Account name to see the current user name and group.

<p align="center"><img
src="/files/2012-12-29-install-and-config-bitlbee-on-mac-os-mountain-lion/uid.png"></p>

Now back to the terminal window and run this command to edit the config file
we've just renamed.

{% highlight sh %}
sudo emacs bitlbee.conf
{% endhighlight %}

Find this line

{% highlight bash %}
# RunMode = Inetd
{% endhighlight %}

and change it to 

{% highlight bash %}
RunMode = ForkDaemon
{% endhighlight %}

Next, find this line

{% highlight bash %}
# User = bitlbee
{% endhighlight %}

and change to 

{% highlight bash %}
User = your-user-name
{% endhighlight %}

Replace **your-user-name** with your username. Actually, when installing bitlee,
it will create a user named bitlbee however I don't know why on my Mac it didn't
so I let run as my username.

The rest just leave it default. Press Ctrl-x Ctrl-s to save the file and then
Ctrl-x Ctrl-c to exit emacs.

Finally, you need to change the owner of the directories that bitlbee uses to
your account. Open Terminal and execute those commands

{% highlight sh %}
sudo chown -R your-user-name:your-group-name /opt/local/etc/bitlbee
{% endhighlight %}
{% highlight sh %}
sudo chown -R your-user-name:your-group-name /opt/local/var/lib/bitlbee
{% endhighlight %}

Replace your-user-name and your-group-name in the commands above with your user
name and group name.

Now you're ready to run **bitlbee** by typing **bitlbee** into terminal and hit
**return**.

> This little f~~king stuff took me more than 1 hours to install and config. Why
> it's so complicated! >.<
