---
layout: post
showtn: 
title: "MacOS - Permanently Disable Reopen Windows when Logging back"
description: ""
category: Misc
thumbnail: /files/thumbnails/macos.png
tags: [macos]
---
{% include JB/setup %}

I really hate the "Reopen Windows when Logging back"" feautre in MacOS Lion and
above. That feature exists because Apple wanted to integrate iOS into MacOS.
However, my Macbook uses a HDD, not an SSD, so everytime I log into my computer,
it automatically opens all those applications, which takes time and can make my
computer run very slowly. Even though I have unchecked the "Reopen windows when
logging back in" option in the Shutdown/Restart dialog, my Mac often reopen all
my last running applications. I have to wait for all of those stuffs to
completed open and then I can use my computer normally.

![Restart Dialog](/files/2013-04-03-macos-permanently-disable-reopen-windows-when-logging-back/disable-reopen-windows.jpg)

<!-- more -->

I was very upset and decided to find a solution to completely disable that
feature. And here is the solution.

Use any text editor that you want and create a file named fixlogin.sh in your
home directory. Paste this piece of code into the content of the file

{% highlight sh %}
#!/bin/bash
echo "#!/bin/bash" > /tmp/loginfix.sh
echo "rm /Users/*/Library/Preferences/ByHost/com.apple.loginwindow.*" >> /tmp/loginfix.sh
mv /tmp/loginfix.sh /usr/bin/loginfix.sh
chmod +x /usr/bin/loginfix.sh
defaults write com.apple.loginwindow LoginHook /usr/bin/loginfix.sh
{% endhighlight %}

Open up terminal, chmod that file to executable and then execute it.

{% highlight console %}
$ cd
$ chmod +x fixlogin.sh
$ sudo ./fixlogin.sh
{% endhighlight %}

Ok, you are done! The checkbox "Reopen windows when logging back in" will still
appear in the shutdown/restart dialog but it has no effect.

Later, when you want to revert back to the behavious of MacOS Lion, issue this
command

{% highlight console %}
$ sudo defaults delete com.apple.loginwindow LoginHook
{% endhighlight %}

----------

Idea from this post: <http://osxdaily.com/2011/08/25/disable-reopen-windows-when-logging-back-in-in-mac-os-x-lion-completely/>
