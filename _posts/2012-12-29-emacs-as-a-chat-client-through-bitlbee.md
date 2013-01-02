---
layout: post
title: "Emacs as a Chat client through Bitlbee"
description: ""
category: 
tags: [emacs, tutorial, bitlbee, chat, erc]
---
{% include JB/setup %}

Emacs is a great text editor, the most wonderful one I've been used :D Also it
has built-in support for ERC chat client. And now I used it as my chat client
instead of Adium and the <span>f**king</span> Yahoo Messenger for Mac.

There are
two way to run a chat client through Bitlbee. First is to connect to a public
Bitlbee server, register an account and start using the services. The second
method is to setup your private Bitlbee server and connect to it. Of course I
choose the second way because the first one is not for the nerds :LOL:. And also
you cannot install or config the server if you're using a public server.

# Install Bitlbee

If you wish to use the easy method or you just want to try **Bitlbee** first,
you can use Bitlbee public server at **im.bitlbee.org:6667** or get one from the
public server list here <http://www.bitlbee.org/main.php/servers.html>.

If you wish to use your own Bitlbee server, first install it from the
[homepage](http://www.bitlbee.org/main.php/news.r.html). If you're using Mac OS
like me, I have an article
[here](/2012/12/29/install-and-config-bitlbee-on-mac-os-mountain-lion/) to
demonstrate how to install and config it on Mac OS Mountain Lion. I see the
instruction is easy but the real situation on my Mac is so complicated >.< I
don't know how the installation prcess happens on other OS so if you have
expreienced, please share with me. After finish installation, you need to run
Bitlbee as Daemon mode by either edit the config file to set **RunMode =
ForkDaemon** or run bitlbee with the argument **-D** like this

{% highlight sh %}
$ bitlbee -D
{% endhighlight %}

# Connect Emacs to Bitlbee server

In Emacs, press M-x, type in **erc** and input server address (localhost if
you installed bitlbee server on your own computer) and port (default is 6667).
If this is the first time you connect to bitlbee server, choose a user and type
in any password that you want.

![Bitlbee localhost](/files/2012-12-29-emacs-as-a-chat-client-through-bitlbee/localhost.png)

Now you're successfully connected bitlbee server. Type in help to show the help
or go to the next article to know how to use ;))

-----

**Next**: Basic Bitlbee usage.
