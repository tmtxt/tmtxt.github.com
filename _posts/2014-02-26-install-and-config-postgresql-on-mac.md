---
layout: post
title: "Install and Config PostgreSQL on Mac"
description: ""
category: misc
tags: [postgres]
---
{% include JB/setup %}

# Installation & Configuration

There are several ways to install PostgreSQL on Mac OS. Take a look at this page
[Mac OS X packages](http://www.postgresql.org/download/macosx/) choose one
method that best suits for you. In this post I will use the Graphic installer as
the demo but you can also use other way, there will be not many differences.

After you have downloaded and installed PostgreSQL using the graphic installer,
there will be a new account named **postgres** and the server will automatically
start. Now you need to add the postgres executable files into your shell's PATH.
Luckily, the graphic installer has generated the configuration for you already.
Just add this line to the end of your shell rc file (.bashrc, .zshrc)

{% highlight sh %}
source /Library/PostgreSQL/9.3/pg_env.sh
{% endhighlight %}

Restart your shell and now those postgres binary files (postgres, pg_ctl,
psql,... ) is ready for use. But there is still one problem, it is the
**postgres** user in your system. You need to update its password so later you
can log into that account. Open **System Preferences** > **Users & Groups** and
then choose Reset Password of the **postgres** account.

To start the server, use this command and then input the password of the
**postgres** account.

{% highlight console %}
$ su postgres -c "pg_ctl start"

Password:
server starting
2014-02-26 10:09:20 ICT LOG:  redirecting log output to logging collector process
2014-02-26 10:09:20 ICT HINT:  Future log output will appear in directory "pg_log".
{% endhighlight %}

<!-- more -->

To stop the server

{% highlight console %}
$ su postgres -c "pg_ctl stop"
{% endhighlight %}

Sometimes, the server fails to shutdown. To immediately disconnects sessions
rather than waiting for session-initiated disconnection, use this one

{% highlight console %}
$ su postgres -c "pg_ctl stop -m fast"
{% endhighlight %}

To restart the server

{% highlight console %}
$ su postgres -c "pg_ctl restart"
{% endhighlight %}

# Extra

You may notice that after installing PostgreSQL, there will be an extra user
account created and it appears on the log-on screen every time you start your
computer. To hide this user, you need to change its UID to a number less than
500 and turn on the option to hide all accounts with UID less than 500.

Firstly, select a UID (&lt;500) you want to assign for this account. Use this
command to determine if the uid is already in use

{% highlight console %}
$ dscacheutil -q user | grep "uid: 235"
uid: 235
uid: 235
$ dscacheutil -q user | grep "uid: 400"
{% endhighlight %}

In the example above, there is no account associated with uid 400 so I will
choose this. Next, execute this command to find the uid of **postgres** account.

{% highlight console %}
$ dscl . -read /Users/postgres UniqueID
UniqueID: 502
{% endhighlight %}

Continue to run this command to change the UID from 502 to 400

{% highlight console %}
$ sudo dscl . -change /Users/postgres UniqueID 502 400
{% endhighlight %}

Finally, this command helps you to hide all users with uid less than 500

{% highlight console %}
$ sudo defaults write /Library/Preferences/com.apple.loginwindow Hide500Users -bool YES
{% endhighlight %}
