---
layout: post
showtn: 
title: "Jekyll - Blogging platform for geeks"
description: ""
category: Jekyll
thumbnail: /files/2012-12-27-jekyll-create-a-list-of-lastest-posts/thumbnail.png
tags: [jekyll]
---
{% include JB/setup %}

# Why we need Jekyll?

Many of you should have known or have used some popular blogging platforms like
Wordpress or Blogspot. They are all great platforms that have been developed for
a long time and have a big community. However, they also have some
disadvantages.

* Have you ever found that Wordpress or Blogspot too complex and too difficult to
backup and restore?  
* Have you ever encountered the problem when you want to display a piece of
programming code in Wordpress, Blogspot,...?  
* Have you ever feel annoyed by the useless code that their WYSIWYG editors
generate? And to fix them you have to edit the markup manually, which is very
complicated for us to focus on the main content.
* Have you ever noticed that they took a long time to load the pages?
* And so on...

If you feel annoyed with these problems, it's time for you to find another
better solution.

<!-- more -->

# What is Jekyll?

> Jekyll is a parsing engine bundled as a ruby gem used to build static websites
> from dynamic components such as templates, partials, liquid code, markdown,
> etc. Jekyll is known as “a simple, blog aware, static site generator”.
  
So what Jekyll do is just to parse your content, mainly written in markdown
text, to a static website. It's also the engine used behind the Github pages.

# Advantages of Jekyll

## It's static and It's fast

As I mentioned before, what Jekyll does is to parse your markdown content to
static html pages. Since it's static, it's really fast. Unlike the other
dynamic platforms, everytime you load the pages, Jekyll server does nothing but
returns you a static html page.  

It's static but it doesn't mean that it does not have the features like
Wordpress or Blogspot. Jekyll has all those blogging features including posts,
commenting system, tagging, archive,...

## Deployment

Since Jekyll is the engine used behind Github, you can have your blog deployed on
Github with no cost. You can even buy your own domain and follow the instruction
on Github to setup Github to host your Jekyll blog with that domain.
Moreover, you can set up your own Jekyll server on your own computer or on your
VPS,... 

## Markdown

Back to the problem I have discussed above, I hate WYSIWYG editors cause they
generate too much unused markup code and even sometimes can break my content
display. To fix it, I have to manually edit the markup code, which is extremely
painful when I have a long post. It makes me feel difficult to concentrate on my
main content.  

Jekyll is different. It lets you use markdown in your posts and then generate the
html pages from the markdown code. Markdown concentrates on the content not the
display like markup. It can be displayed even in plain text and the readers still
can understand and easy focus on what the content is. Moreover, it's very easy
for you to get used to.

## Code Highlighting

Who are we?
We are IT guys so that Code sharing is an essential part of our life. We all
know that Github handle this task very well and so do Jekyll. It has the
built-in support for Pygments code highlighting system. After some simple
configurations (you can find it on Google or contact me), this is the result I
have for a piece of js code

![Js code](/files/2013-01-16-jekyll-bootstrap-blogging-platform-for-geeks/js.png)

Really beautiful, isn't it? What you need to do is just as simple as copy and
paste the code. Jekyll will keep the format of the code and highlight it for
you. If you use markup to write this, it's really a big problem for you to deal
with the tabs and spaces as well as the line breaks.

## Version Control

This is the most important feature of Jekyll. I changed to Jekyll mainly because
of this.

Jekyll uses no database. The website is just a collection of text files so that
it's extremely easy to backup and manage. You don't have to worry about backing
up and restoring the site code, theme, posts, database,... if you have done
anything wrong. Since they are all text files so you can use **git** to manage all
the code, themes as well as posts and pages. It also help you publish websites
from terminal by typing the **push** command. If you use Github as the website
host, it's even more convinence because Github itself uses git.  

If you use a powerful text editor, such as emacs, you can find many packages that
help you in managing code version with git in a very convenience and elegant way
(**magit** in my case).

## Extensible

Jekyll has a growing community. You can find many plugins that help you extend
your Jekyll blog ability like emoticon, video embedding,...

# Reference

Jekyll Bootstrap Homepage: [http://jekyllbootstrap.com/](http://jekyllbootstrap.com/)  
Ruhoh (another static blog generator): [http://ruhoh.com/](http://ruhoh.com/)  
Octopress (another framework based on Jekyll): [http://octopress.org/](http://octopress.org/)  
Markdown in Jekyll:
[https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#wiki-code](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#wiki-code)  
Sites built with Jekyll: [https://github.com/mojombo/jekyll/wiki/Sites](https://github.com/mojombo/jekyll/wiki/Sites)
