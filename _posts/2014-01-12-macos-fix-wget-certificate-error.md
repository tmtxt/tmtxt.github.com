---
layout: post
title: "MacOS Fix wget certificate error"
description: ""
category: misc
tags: [macos, wget]
---
{% include JB/setup %}

On OSX, when I use **wget** for downloading file from https source, I got the
Certificate issue error.

{% highlight console %}
Resolving fbcdn-profile-a.akamaihd.net (fbcdn-profile-a.akamaihd.net)... 23.2.16.8, 23.2.16.41, 23.2.16.32, ...
Connecting to fbcdn-profile-a.akamaihd.net (fbcdn-profile-a.akamaihd.net)|23.2.16.8|:443... connected.
ERROR: The certificate of ‘fbcdn-profile-a.akamaihd.net’ is not trusted.
ERROR: The certificate of ‘fbcdn-profile-a.akamaihd.net’ hasn't got a known issuer.
{% endhighlight %}

The reason for this problem is that the default certificate directory is
hard-coded in **wget** as **/etc/ssl/certs**, which corresponds to the Linux
directory layout and doesn't exist on Mac OS.

To fix this, you can either tell wget to skip certificate check or fix the
certificate error in Mac OS. For the first solution, just add
`--no-check-certificate` to the `wget` command when you start it. For example

{% highlight console %}
$ wget --no-check-certificate http://example.com
{% endhighlight %}

If you want to fix the problem, first, install **curl-ca-bundle** from Macports

{% highlight console %}
$ sudo port install curl-ca-bundle
{% endhighlight %}

Next, edit your **~/.wgetrc** file (create a new one if it's not exist yet) and
add this line to the end of the file

{% highlight sh %}
CA_CERTIFICATE=/opt/local/share/curl/curl-ca-bundle.crt
{% endhighlight %}

If you have followed my previous post on
[How to install and run Macports form home directory]({%post_url 2013-11-08-macports-from-home-directory%}),
then the link to **curl-ca-bundle** should be

{% highlight sh %}
CA_CERTIFICATE=~/macports/share/curl/curl-ca-bundle.crt
{% endhighlight %}

<!-- more -->
