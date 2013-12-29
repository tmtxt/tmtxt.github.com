---
layout: post
title: "Shell command in Conkeror on Mac with conkeror_mac_bundler"
description: ""
category: conkeror
tags: [conkeror, conkeror_mac_bundler, mac]
---
{% include JB/setup %}

There are several ways of installing Conkeror on Mac OS X but I prefer using the
**conkeror_mac_bundle** script for quickly building the Conkeror application and
easily setting it as default browser on Mac OS. For instruction on how to
install Conkeror using this method, refer to this page
[Installation Instructions for OS X](http://conkeror.org/InstallationOSX).

The only problem with **conkeror_mac_bundler** script is that you are not able
to run shell command directly in Conkeror. The reason is because it cannot find
`conkeror-spawn-helper` in the load path. However, `conkeror-spawn-helper` is
already included when the script build the Conkeror bundle. You can find it
under the folder **Contents/MacOS** inside the **Conkeror.app** bundle.

Once you find it, add this code to your .conkerorrc file and change the path
corresponding to the real location on your computer

{% highlight js %}
PATH.push("~/Applications/conkeror_mac_bundler/Conkeror.app/Contents/MacOS");
{% endhighlight %}

That's everything you need to do. Restart Conkeror and try it by pressing
**M-x** and then **shell-command**. You can also use TAB completion there.

![Alt Text](/files/2013-12-29-shell-command-in-conkeror-on-mac-with-conkeror_mac_bundler/sc1.png)

![Alt Text](/files/2013-12-29-shell-command-in-conkeror-on-mac-with-conkeror_mac_bundler/sc2.png)

<!-- more -->
