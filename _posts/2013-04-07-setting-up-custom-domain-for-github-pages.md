---
layout: post
showtn: yes
title: "Setting up Custom domain for Github Pages"
description: ""
category: Jekyll
thumbnail: /files/thumbnails/github.png
tags: [github, jekyll]
---
{% include JB/setup %}

Yesterday, I decided to buy my own domain to serve for my blog after months
using github.com subdomain (<http://tmtxt.github.com>). It took me a
little difficult to figure out how to setup my custom domain (bought from
namecheap.com) for my old github blog. This is the steps how to config my
current domain <http://truongtx.me>.

First you need to config the domain in your repo. Create a file name CNAME in
your root directory of your blog. Insert your custom domain there, in my case it
it **truongtx.me**. Save the file and push it to github.

Now open namecheap.com and login into your account and go to Domain management.
Open Host Records to edit your domain to point to github. Follow the config in
the picture below and then click Save. **204.232.175.78** is the ip of github
server, you can find it in this
[page](https://help.github.com/articles/setting-up-a-custom-domain-with-pages)

![namecheap](/files/2013-04-07-setting-up-custom-domain-for-github-pages/namecheap.png)

<!-- more -->

It can take up to a whole day for the new domain to take effect, be patient!
