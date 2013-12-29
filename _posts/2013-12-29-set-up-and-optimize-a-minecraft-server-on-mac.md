---
layout: post
title: "Set up and Optimize a Minecraft server on Mac"
description: ""
category: misc
tags: [minecraft, macos]
---
{% include JB/setup %}

![minecraft](/files/2013-12-29-set-up-and-optimize-a-minecraft-server-on-mac/minecraft.jpg)

# Set up Minecraft server

First, download the Minecraft Multiplayer Server executable jar file from its
download page here
[https://minecraft.net/downloadLink](https://minecraft.net/download). When you
finish, place the .jar file inside a folder named **minecraft** in your home
directory. Open Terminal, cd to the **minecraft** directory you've just created
before and execute this command to start a Minecraft server

{% highlight console %}
$ java -Xmx1024M -Xms1024M -jar minecraft_server.jar
{% endhighlight %}

If you want to run it without GUI, add `nogui` argument at the end of the command.
For the first time, it will generate the game contents and place them inside the
**minecraft** folder above. After it have finished generating, stop the server
and open the **minecraft** directory before, you will see a file named
`server.properties` there. All the server properties are described here
[http://minecraft.gamepedia.com/Server.properties](http://minecraft.gamepedia.com/Server.properties).
Take a look at that page and config the server to what you want. If you don't
want the server to authenticate username with Minecraft server, change
`online-mode` to `false` to prevent this.

<!-- more -->

When you finish, just execute the
above command again every time you want to start the server. For the client, if
they want to connect to your server, just select **Direct Connect** in the game
menu and type in your server IP and port.

**Note**: for my experience, you may need an SSD for running the server
smoothly. I have tried on 2 other computers running normal laptop HDD and it
results in serious lags in game play. If you are using HDD, head to the next section.

# Running Minecraft server on Ram disk

If your Mac is operating on a HDD or if your SSD cannot serve a large number of
players, you need another faster solution. We will create a small virtual hard
disk located on RAM.

Open Terminal and issue this command for creating a Ram Disk mounted at
**/Volumes/ramdisk**. For other OS, see the instruction here
[http://minecraft.gamepedia.com/Tutorials/Ramdisk_enabled_server](http://minecraft.gamepedia.com/Tutorials/Ramdisk_enabled_server).

{% highlight console %}
$ diskutil erasevolume HFS+ "ramdisk" `hdiutil attach -nomount ram://1165430`
{% endhighlight %}

Open Finder and you will see a new hard disk named **ramdisk**. Just copy the
game server directory there and start the server normally. But remember to copy
it back to your hark disk after exiting the server because all the data in ram
disk will be lost when you eject it or power of your computer.

To automate all the tasks, create a bash script file, for example
**minecraft.sh**, with the content like this. Remember to put the **minecraft**
folder inside your home directory, otherwise, you need to change the content a
little bit.

{% highlight sh %}
#! /usr/bin/env sh

diskutil erasevolume HFS+ "ramdisk" `hdiutil attach -nomount ram://1165430`
rsync -arvz --progress --delete ~/minecraft /Volumes/ramdisk/
cd /Volumes/ramdisk/minecraft
java -Xms512M -Xmx512M  -jar minecraft_server.1.7.4.jar
rsync -arvz --progress --delete /Volumes/ramdisk/minecraft ~/
diskutil unmount ramdisk
{% endhighlight %}

Now, change the permission of the file to executable

{% highlight console %}
$ chmod +x minecraft.sh
{% endhighlight %}

Finally, run it the script and wait for the server to start. All the tasks
including copy back after server quitting are done automatically.

{% highlight console %}
$ ./minecraft.sh
{% endhighlight %}
