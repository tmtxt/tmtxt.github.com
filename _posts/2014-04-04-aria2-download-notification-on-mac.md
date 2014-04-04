---
layout: post
title: "Aria2 download notification on Mac/Linux"
description: ""
categories: [misc]
tags: []
---
{% include JB/setup %}

# Download events in aria2

In my previous post, I have demonstrated how to use 
[aria2 as the default download manager]({%post_url 2013-11-12-aria2-as-default-download-manager%})
on Unix/Linux and how to
[integrate aria2 into Conkeror]({%post_url 2014-03-12-integrate-aria2-with-conkeror%}).
In this post, I'm going to show a simple tip that can cause aria2 to display
notification when it finishes downloading. By default, aria2 provides some input
arguments which are the event handlers for download hooks. Those events include
`--on-bt-download-complete`, `--on-download-complete`, `--on-download-error`,
`--on-download-pause`, `--on-download-start`, `--on-download-stop`. The solution
is to pass a shell script to the event and use that bash script to activate
notification program. For example

{% highlight console %}
$ aria2c --on-download-complete="/path/to/notification/script.sh" http://example.com/file.iso
{% endhighlight %}

You can also pass the event handler command to the aria2 command for starting it
as daemon or on RPC protocol. If you are using the command from my previous
[post]({%post_url 2013-11-12-aria2-as-default-download-manager%}), the command
will look like this

{% highlight console %}
$ touch /path/to/download/folder/session.txt && aria2c --enable-rpc --rpc-listen-all --save-session=/path/to/download/folder/session.txt --input-file=/path/to/download/folder/session.txt -x16 -s16 -k1M --dir=/path/to/download/folder --daemon --on-download-complete=/path/to/notification/script.sh
{% endhighlight %}

When aria2 finishes downloading, it will run the **--on-download-complete**
script and pass 3 arguments, the third one is the path to the downloaded file.
The content of the notification script.sh file should look similar to this

{% highlight sh %}
#!/bin/sh
notification-command "Download complete $3"
{% endhighlight %}

<!-- more -->

# Notification on Linux

Most Linux distros come with the standard APIs for displaying notification. To
activate it, simply execute the command `notify-send "message"` in terminal. The
content of the notification script.sh file is similar to this

{% highlight sh %}
#!/bin/sh
notify-send "Download complete $3"
{% endhighlight %}

You can pass extra arguments to `notify-send` for specifying expiry time or
notification icon.

![Lubuntu](/files/2014-04-04-aria2-download-notification-on-mac/lubuntu.png)

![Xubuntu](/files/2014-04-04-aria2-download-notification-on-mac/xubuntu.png)

# Notification on Mac

Mac OS does not come with a shell application for activating notification.
However, you can use [Growl](http://growl.info/) or
[terminal-notifier](https://github.com/alloy/terminal-notifier) for displaying
notification using Mac's default Notification Center.
**terminal-notifier** can be installed using
[Macports]({%post_url 2013-01-01-macports-the-macos-package-manager%}), ruby or
homebrew

{% highlight console %}
$ port install terminal-notifier
$ gem install terminal-notifier
$ brew install terminal-notifier
{% endhighlight %}

Next, edit the notification script.sh file like this

{% highlight sh %}
#!/bin/sh
terminal-notifier -message "Download completed - $3" -title "Aria2"
{% endhighlight %}

To set the icon for the notification, you need to create an empty aria2 .app
wrapper.

{% highlight console %}
$ sudo mkdir -p /Applications/aria2.app/Contents/MacOS
$ sudo touch /Applications/aria2.app/Contents/MacOS/aria2.sh
$ sudo touch /Applications/aria2.app/Contents/Info.plist
{% endhighlight %}

Edit the content `Info.plist` file

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleExecutable</key>
	<string>aria2.sh</string>
	<key>CFBundleIdentifier</key>
	<string>com.example.aria2</string>
</dict>
</plist>
{% endhighlight %}

Open **Applications** folder in Finder, view info of **aria2.app**, drag and
drop the image you want to the application icon

![Alt Text](/files/2014-04-04-aria2-download-notification-on-mac/mac.png)

Now, back to the notification script.sh file and change it to

{% highlight sh %}
#!/bin/sh
terminal-notifier -message "Download completed - $3" -title "Aria2" -sender "com.example.aria2"
{% endhighlight %}

![Notification Mac](/files/2014-04-04-aria2-download-notification-on-mac/nc.png)
