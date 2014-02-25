---
layout: post
title: "Mac OS Install python, pip, virtualenv using Macports"
description: ""
category: misc
tags: [macos, python, pip, virtualenv, macports]
---
{% include JB/setup %}

Installing python and all its stuff maybe a bit confusing if you're installing
it using Macports
([MacPorts - The MacOS package manager]({%post_url 2013-01-01-macports-the-macos-package-manager%})).
By default, Macports comes with many python ports, e.g., python24, python27,
python34. Each of them is a separate version of python (2.4, 2.7, 3.4,...). It
provides the users the ability to install and maintain multiple version of
python at the same time.

To browse all versions of python provided by Macports, use this command

{% highlight console %}
$ port search python
python26 @2.6.9 (lang)
    An interpreted, object-oriented programming language
python27 @2.7.6 (lang)
    An interpreted, object-oriented programming language
{% endhighlight %}

To install a specific version of python

{% highlight console %}
$ port install python24 python27
{% endhighlight %}

When you install python through Macports, it will auto install the python_select
port. This is a tool for switching among python versions. To view all the the
installed python versions, execute this one

<!-- more -->

{% highlight console %}
$ port select --list python
Available versions for python:
	none
	python24
	python25-apple
	python26-apple
	python27 (active)
	python27-apple
{% endhighlight %}

To set one version as the default one, use `port select` again

{% highlight console %}
$ port select --set python python27
Selecting 'python27' for 'python' succeeded. 'python27' is now active.
{% endhighlight %}

Similarly, before installing pip, search for all available versions of pip

{% highlight console %}
$ port search pip
py25-pip @1.3.1_1 (python, www)
    A tool for installing and managing Python packages.

py26-pip @1.5.4 (python, www)
    A tool for installing and managing Python packages.

py27-pip @1.5.4 (python, www)
    A tool for installing and managing Python packages.
{% endhighlight %}

Select the version of pip corresponding to the version of python that you are
using and then install it

{% highlight console %}
$ port install py27-pip
{% endhighlight %}

Again, you need to use `port select` to set the version of pip to use. Select
the version corresponding to your running python version

{% highlight console %}
$ port select --list pip
Available versions for pip:
	none
	pip24
	pip27 (active)
$ port select --set pip pip27
Selecting 'pip27' for 'pip' succeeded. 'pip27' is now active.
{% endhighlight %}

Now, **python** and **pip** have been installed successfully. Make sure that the
Macports bin is in your PATH and stands before the default Mac OS one. When run
the command `which`, it should be something similar to this

{% highlight console %}
$ which python
/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/python
$ which pip
/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/pip
{% endhighlight %}

Now the best way to install **virtualenv** is to using **pip**

{% highlight console %}
$ pip install virtualenv
{% endhighlight %}
