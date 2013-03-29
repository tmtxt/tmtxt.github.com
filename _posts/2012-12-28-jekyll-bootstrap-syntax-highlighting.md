---
layout: post
title: "Jekyll Bootstrap - Syntax highlighting"
description: ""
category: Jekyll
thumbnail: /files/2012-12-28-jekyll-bootstrap-syntax-highlighting/thumbnail.png
tags: [jekyll, syntax-highlighting, tutorial]
---
{% include JB/setup %}

Jekyll is a blog for hacker so of course it must have syntax highlighting. There are several ways to activate syntax highlighting in Jekyll. I'll show you 2 popular ways that many people use.

# 1. Highlight.js

This is a very easy way to have syntax highlighting in jekyll because it works automatically. That means you don't have to do anything. Once you have include **highlight.js**, it will auto find code blocks, detect language and highlight them for you.

### Advantages:

* Easy to use  
* Easy to update (just replace the js file or even auto update)

### Disadvantages:

* Not very-well syntax highlighting but acceptable
* You will have to format the code blocks manually (indent, new line,...)

### Information

**Highlight.js homepage**: <http://softwaremaniacs.org/soft/highlight/en/>  
**Demo**: <http://softwaremaniacs.org/media/soft/highlight/test.html>

### Installation

There are to ways to use **Highlight.js**. First is to download the package, extract it into your website directory and include this in your template file (usually /_includes/themes/theme-name/default.html). Remember to edit link to point to the right position.

{% highlight html %}
<link rel="stylesheet" href="styles/default.css">
<script src="highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
{% endhighlight %}

The second method is much faster and easier. **Highlight.js** is also hosted at Yandex so you even don't need to download and install it. Just include this in your template file (usually /_includes/themes/theme-name/default.html) and you're done ;)

{% highlight html %}
<link rel="stylesheet" href="http://yandex.st/highlightjs/7.3/styles/default.min.css">
<script src="http://yandex.st/highlightjs/7.3/highlight.min.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
{% endhighlight %}

### Usage

Just indent your code blocks by one tab or at least 4 spaces. The rest is automatically done for you.

# 2. Pygments (recommended)

Jekyll has built-in support for syntax-highlighting via Pygments. If you want to use it, make sure you run jekyll with Pygments support. I'll show how later.

### Advantages:

* Beautiful syntax and code highlighting with support for more then 100 languages
* Don't have to worry about the code blocks's format. Just copy and paste from the source code.

### Disadvantages:

* Not as easy to install as highlight.js (but I will demonstrate how LOL)
* Can not be auto-update (but may change in the future)

### Information

**Pygments on github**: <https://github.com/mojombo/jekyll/wiki/Liquid-Extensions>  
**Pygments homepage**: <http://pygments.org/>  
**Supported languages list**: <http://pygments.org/languages/>

### Installation

#### Install pygments on local computer

* On Ubuntu, just one command
{% highlight sh %}
$ sudo apt-get install python-pygments
{% endhighlight %}
* On Mac OS  
Check the python version
{% highlight sh %}
$ python --version  
Python 2.7.2
{% endhighlight %}
Download the Setuptools package [here](http://pypi.python.org/pypi/setuptools#files). Select the package corresponding to your python version.  
Next, cd to the directory you've just donwloaded it and run this command
{% highlight sh %}
$ sudo sh setuptools-0.6c11-py2.6.egg
{% endhighlight %}
Install pygments
{% highlight sh %}
$ sudo easy_install Pygments
{% endhighlight %}

#### Enable pygments

In the config.yaml file inside the root directory of your jekyll website, set pygments to true

    pygments: true

#### Generate pygments

{% highlight sh %}
$ cd path/to/jekyll/project/folder
$ pygmentize -S default -f html > pygments.css
{% endhighlight %}
It will generate a file named pygments.css in your website root directory.

#### Include pygments.css

The last step is to include the css file in your template file (usually /_includes/themes/theme-name/default.html). Put this code in the head area of you template

{% highlight html %}
<link rel="stylesheet" href="/pygments.css">
{% endhighlight %}

### Usage

    {{ "{% highlight language "}}%}  
	   your code goes here  
	{{ "{% endhighlight "}}%}

Replace language with the language name (ruby, C, javascript,...).
