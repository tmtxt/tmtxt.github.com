---
layout: post
title: "Magit for non emacs-users"
description: ""
category: Emacs
thumbnail: 
showtn: no
tags: [magit, emacs]
---
{% include JB/setup %}

[Magit](https://github.com/magit/magit)
is one of the best tool for working with git. However, it's a package ship
with Emacs, not a standalone application, which prevents many non-emacs people
from using it because they don't want to deal with all the configuartion in the
init file. Luckily, there is a python script called **magit-wrapper**
[https://github.com/ubolonton/magit-wrapper](https://github.com/ubolonton/magit-wrapper),
which can help that kind of people and act as a gateway drug to Emacs. Moreover,
Emacs users can use it for working with files on a remote server to avoid having
to install all the Emacs GUI and the full user init **.emacs.d** folder.

![Screenshot](/files/2013-07-10-magit-for-non-emacs-users/ss.png)

<!-- more -->

This is for Emacs in terminal version 23+. OSX's default one is 22. To check for
the version of Emacs that you are using, simply type

{% highlight console %}
$ emacs --version
{% endhighlight %}

To use **magit-wrapper**, first you need to clone the repository into your local
computer

{% highlight console %}
$ git clone --recursive https://github.com/ubolonton/magit-wrapper.git /path/to/magit-wrapper
{% endhighlight %}

After finishing cloning, `cd` to your project folder, run

{% highlight console %}
$ /path/to/magit-wrapper/magit.py
{% endhighlight %}

For the convinience, you should make a symbolic link to the **magit.py** file to
somewhere in PATH variable so that your shell can easily find it and you don't
have to type all the path to **magit.py** everytime you need need to use it

{% highlight console %}
$ sudo ln -s /absolute/path/to/magit-wrapper/magit.py /usr/local/bin/magit
{% endhighlight %}

Now, instead of typing `/path/to/magit-wrapper/magit.py`, you can just simply
type in

{% highlight console %}
$ magit
{% endhighlight %}

This is just a temporary solution. For the best experiment, start to learn how
to use Emacs :D

The author of this is [Mr. Nguyen Tuan Anh](https://github.com/ubolonton). You
can contact him at [ublonton@gmail.com](ubolonton@gmail.com)
