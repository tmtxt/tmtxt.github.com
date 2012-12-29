---
layout: post
title: "Jekyll server - Fix Server error - Address already in used"
description: ""
category: 
tags: []
---
{% include JB/setup %}

Sometimes when you run Jekyll server locally to test the preview of your site,
you will encounter an error that tells you **Address is already in used**. The
error output in the console when you run the command **jekyll --server** is
something similar to this

{% highlight bash %}
/Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/lib/ruby/site_ruby/1.9.1/rubygems/custom_require.rb:36:in `require': iconv will be deprecated in the future, use String#encode instead.
Configuration from /Users/tranxuantruong/tommytxtruong.github.com/_config.yml
Auto-regenerating enabled: /Users/tranxuantruong/tommytxtruong.github.com -> /Users/tranxuantruong/tommytxtruong.github.com/_site
[2012-12-30 00:10:58] regeneration: 119 files changed
[2012-12-30 00:10:58] INFO  WEBrick 1.3.1
[2012-12-30 00:10:58] INFO  ruby 1.9.3 (2012-11-10) [x86_64-darwin12.2.0]
[2012-12-30 00:10:58] WARN  TCPServer Error: Address already in use - bind(2)
[2012-12-30 00:10:58] WARN  TCPServer Error: Address already in use - bind(2)
/Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/lib/ruby/1.9.1/webrick/utils.rb:85:in `initialize': Address already in use - bind(2) (Errno::EADDRINUSE)
	from /Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/lib/ruby/1.9.1/webrick/utils.rb:85:in `new'
	from /Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/lib/ruby/1.9.1/webrick/utils.rb:85:in `block in create_listeners'
	from /Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/lib/ruby/1.9.1/webrick/utils.rb:82:in `each'
	from /Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/lib/ruby/1.9.1/webrick/utils.rb:82:in `create_listeners'
	from /Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/lib/ruby/1.9.1/webrick/server.rb:82:in `listen'
	from /Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/lib/ruby/1.9.1/webrick/server.rb:70:in `initialize'
	from /Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/lib/ruby/1.9.1/webrick/httpserver.rb:45:in `initialize'
	from /Users/tranxuantruong/.rvm/gems/ruby-1.9.3-p327/gems/jekyll-0.12.0/bin/jekyll:288:in `new'
	from /Users/tranxuantruong/.rvm/gems/ruby-1.9.3-p327/gems/jekyll-0.12.0/bin/jekyll:288:in `<top (required)>'
	from /Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/bin/jekyll:23:in `load'
	from /Users/tranxuantruong/.rvm/rubies/ruby-1.9.3-p327/bin/jekyll:23:in `<main>'
{% endhighlight %}

That means there's already some other process or jekyll running on the port that
jekyll uses and
because of some reasons they do not close automatically. To fix this, open
Terminal and run this command to list all process running at port 4000 (jekyll's
default port)

{% highlight bash %}
lsof -wni tcp:4000
{% endhighlight %}

After that issue this command

{% highlight bash %}
kill PID
{% endhighlight %}

Replace PID with the PID of the running process. Now run the jekyll server
again. ;)
