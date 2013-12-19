---
layout: post
title: "Blogging using your favorite Text editor with Git and Jekyll - Part 1"
description: ""
category: Jekyll
thumbnail: 
showtn: yes
tags: [git, jekyll, blog]
---
{% include JB/setup %}

In my previous post
[Jekyll - Blogging platform for geeks]({% post_url 2013-01-16-jekyll-bootstrap-blogging-platform-for-geeks %}),
I have introduced Jekyll as well as its interesting features. This time, I wrote
this post as a tutorial for those who want to build a static website/blog using
Jekyll Bootstrap (an pre-implemented website in Jekyll), Git and hosted it on
Github for free. Also, you can deploy it on your
own server or use Github to host it with your custom domain.

# Requirement

* As I mentioned above, Jekyll is a blogging platform for geeks, not for newbie.
You need to have basic knowledge about web programming and version control.

* You also need to have your computer running Linux, MacOS, Unix or some
similar operating system. You can still run Jekyll on Windows but it's not
encouraged and may cause some issue for you
([Jekyll Documentation](http://jekyllrb.com/docs/installation/))

* Choose a text editor that suitable for you. You can use any text editor that
you. However, I recommend Emacs, Vim and SublimeText. In this article, I'll
demonstrate using Emacs (because I use Emacs) but it's up to you.

* The last requirement is to have [Ruby](http://www.ruby-lang.org/en/downloads/),
[RubyGems](http://rubygems.org/pages/download) and [Git](http://git-scm.com/)
installed on your computer.

<!-- more -->

# Setting up

## Jekyll server

You don't need Jekyll server if you want to host your website on Github (since
Jekyll is the engine behind Github). However,
having Jekyll installed in your computer can help you preview your site before
publishing it to Github. Also, if you want to deploy your site on another
server, you have to install Jekyll to generate the site.

Install it with gem

{% highlight console %}
$ gem install jekyll
{% endhighlight %}

For more information, see the
[Jekyll Installation Documentation](http://jekyllrb.com/docs/installation/).

## Jekyll Bootstrap

Since you're not familiar with Jekyll, we'll use Jekyll Bootstrap. It's like a
Jekyll site template using Twitter Bootstrap that can help you quickly create
and publish your blog onto Github. It provides you a complete static blog with
Pages, Categories, Tags and commenting system. Later, when you get used to
blogging with Jekyll, you can customize everything to make it yours.

Go to your <https://github.com> and create a new repository named
USERNAME.github.com (USERNAME is your github username).

Clone and then push Jekyll Bootstrap onto your newly created repo and your blog is ready to
serve at the address <http://USERNAME.github.io> (before it was
<http://USERNAME.github.com> but changes to .io now for security reason).

{% highlight console %}
$ git clone https://github.com/plusjade/jekyll-bootstrap.git USERNAME.github.com
$ cd USERNAME.github.com
$ git remote set-url origin git@github.com:USERNAME/USERNAME.github.com.git
$ git push origin master
{% endhighlight %}

It can take up to 10 minutes for your site ready to serve for the first time, be patient!

To preview your site locally, cd to the site directory and run Jekyll

{% highlight console %}
$ cd USERNAME.github.com
$ jekyll --server
{% endhighlight %}

**Update**: if you are using Jekyll 1.0+, run this command instead to activate
Jekyll server

{% highlight console %}
$ cd USERNAME.github.com
$ jekyll serve
{% endhighlight %}

## Markdown mode

Jekyll is designed to work with markdown. It stores most of your content in
markdown and then parse the data into html. You can still create your your
content in html form for some special cases (will be discussed later). However,
using markdown is faster and much easier to compose. Spend about 5 minutes to get
used with markdown syntax here
[http://daringfireball.net/projects/markdown/syntax](http://daringfireball.net/projects/markdown/syntax).
It's really easy to understand.

I'm pretty sure that all those 3 advanced text editors that I suggested before
have plugins (or built-in support) to help you work with markdown mode
properly.

For emacs user, you can install markdown mode and yasnippet using package.el
(follow this
[instruction]({% post_url 2013-01-07-emacs-package-manager %})) to compose in
markdown mode faster.

# Start your first content

First, cd to the root directory of your site

{% highlight console %}
$ cd USERNAME.github.com
{% endhighlight %}

To create a new post

{% highlight console %}
$ rake post title="Hello World"
{% endhighlight %}

Create a page

{% highlight console %}
$ rake page name="about.md"
{% endhighlight %}

For Emacs user, you don't need to open terminal to execute those command since
Emacs has its own terminal emulator. Just open that directory in Emacs and press s-L
(shell-command) and run the command you want.

All posts will be store in the **\_posts** directory inside the site's
root directory. The blog post is just a plain text file writtent in markdow
mode. Open one file in your favorite editor and start composing (in markdown).
To delete one post, simply delete the post file.

Finally, run `jekyll --server` or `jekyll serve` (as shown before) to preview your site locally.

More information here:
[Jekyll Quick Start](http://jekyllbootstrap.com/usage/jekyll-quick-start.html)

# Publishing your content

You have finished composing your first post, now it's time to publish your content. What you
need to do is to commit and push the changes to Github and the Jekyll server
there will do the rest.

{% highlight console %}
$ git add .
$ git commit -m "Add new content"
$ git push origin master
{% endhighlight %}

# Summary

Your blog is now ready to serve the public. Jekyll Bootstrap also includes some
basic pages for Category, Tagging and Commenting system (we need to
customize it later).

But wait, that's not enough. It still lacks some basic features of a blog
like Image embedding, Syntax highlighting,... In the next part, I'll
continue with those advanced features.

**Part 2**: [Blogging using your favorite Text editor with Git and Jekyll - Part 2]({% post_url 2013-05-09-blogging-using-your-favorite-text-editor-with-git-and-jekyll %})

Visit the [Jekyll section](/categories.html#Jekyll-ref)
for more tips in making your Jekyll site more attracting.
