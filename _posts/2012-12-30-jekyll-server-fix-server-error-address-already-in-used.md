---
layout: post
showtn: yes
title: "Mac OS - Fix Jekyll error: Address already in used"
description: ""
category: Jekyll
thumbnail: /files/2012-12-27-jekyll-create-a-list-of-lastest-posts/thumbnail.png
tags: [jekyll]
---
{% include JB/setup %}

Sometimes when you run Jekyll server locally to test the preview of your site,
you will encounter an error that tells you **Address is already in used**. The
error output in the console when you run the command **jekyll --server** is
something similar to this

{% highlight console %}
[2012-12-30 00:10:58] INFO  ruby 1.9.3 (2012-11-10) [x86_64-darwin12.2.0]
[2012-12-30 00:10:58] WARN  TCPServer Error: Address already in use - bind(2)
[2012-12-30 00:10:58] WARN  TCPServer Error: Address already in use - bind(2)
{% endhighlight %}

<!-- more -->

That means there's already some other process or jekyll running on the port that
jekyll uses and
because of some reasons they do not close automatically. To fix this, open
Terminal and run this command to list all process running at port 4000 (jekyll's
default port)

{% highlight console %}
$ lsof -wni tcp:4000
{% endhighlight %}

After that issue this command

{% highlight console %}
$ kill PID
{% endhighlight %}

Replace PID with the PID of the running process. Now run the jekyll server
again. ;)
