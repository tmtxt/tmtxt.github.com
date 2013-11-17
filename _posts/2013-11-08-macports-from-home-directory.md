---
layout: post
title: "Macports from home directory"
description: ""
category: misc
thumbnail: 
showtn: no
tags: [macports, macos]
---
{% include JB/setup %}

Macports is one of the most popular package manager system for MacOS. Usually,
when you want to install Macports, you will download the automatic installer
from Macports website. This is convenient for those people who first come to
Macports's world. Usually, the best way to backup with Mac
is to backup your home directory. However, Macports by default will install all
its packages
into **/opt/local**, which make it difficult when you want to re-install your
whole system or migrate to a new Mac because it's outside your home directory.

The solution is to install Macports into your home directory and config it to
put all its stuff into your home directory, too. By using this method, you can
easily backup your home directory and start your work immediately with all your
favorite applications/packages when you re-install the whole system.

There is one thing to notice. Not all packages from Macports can be installed
and launched from your home directory. The ones that cannot are those that need
to create another user, interact with folders outside your home dir,... (tasks
that need sudo permission). One example that I have encountered is Mysql server.
It needs to create another user to run the instance of Mysql but I think we can
config it (I have not tried yet). To fix this, simply install 2 instances of
Macports, one using the default installer and one in your home directory.

<!-- more -->

# Install Macports to Home directory

To install Macports to home directory, you need to download its source code and
compile it yourself. First, download the source as bzip2 tarball at
<http://www.macports.org/install.php> and unzip it. After that, open up Terminal
and cd to that directory. Use this command to config, compile and install
Macports.

{% highlight console %}
$ PATH=/usr/bin:/usr/sbin:/bin:/sbin ./configure --enable-readline --prefix=$HOME/macports --with-install-user=`id -un` --with-install-group=`id -gn` --with-tclpackage=$HOME/macports/share/macports/Tcl 
$ make
$ make install
{% endhighlight %}

This will install Macports into the folder **~/macports**. In order to use it,
you need to load it into shell PATH. Open your shell rc file (.bashrc,
.zshrc,...) and add

{% highlight sh %}
export PATH=$HOME/macports/bin:$HOME/macports/sbin:$PATH
export MANPATH=$HOME/macports/share/man:$MANPATH
export PERL5LIB=$HOME/macports/lib/perl5/5.12.4:$HOME/macports/lib/perl5/vendor_perl/5.12.4:$PERL5LIB
{% endhighlight %}

Now, to test your new Macports installation, open up Terminal ad type

{% highlight console %}
$ which port
{% endhighlight %}

You may consider adding alias for it so that it's easier for you to distinguish
all your Macports instances.

{% highlight sh %}
alias port-home='$HOME/macports/bin/port'
alias port-system='sudo /opt/local/bin/port'
{% endhighlight %}

# Install packages from Macports in home dir

Now, it's time to test your Macports which you have installed before into your
home folder. Open Terminal and try to install some packages from it. For
example

{% highlight console %}
$ port-home install gnupg
$ which gpg
{% endhighlight %}

For most packages from Macports, usually, they can be installed and launched
from your home directory. Only some special packages, which create extra user
account or interact with folders that need root permission (mysql server for
example). In that case, use the **port-system** to install them.
