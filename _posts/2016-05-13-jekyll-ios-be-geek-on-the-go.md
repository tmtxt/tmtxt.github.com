---
layout: post
title: "Jekyll on iOS - Be the geek on the go"
description: ""
categories: [misc]
tags: []
thumbnail: /files/2016-05-08-jekyll-ios-be-geek-on-the-go/bw1.jpg
---
{% include JB/setup %}

[wc]: /files/2016-05-08-jekyll-ios-be-geek-on-the-go/wc.jpg
[bw1]: /files/2016-05-08-jekyll-ios-be-geek-on-the-go/bw1.jpg
[bw2]: /files/2016-05-08-jekyll-ios-be-geek-on-the-go/bw2.jpg
[cd]: /files/2016-05-08-jekyll-ios-be-geek-on-the-go/cd.jpg
[sa]: /files/2016-05-08-jekyll-ios-be-geek-on-the-go/sa.jpg

> This post is written entirely on iOS on my iPhone and iPad, from many places, at several times, in different situations.

- So... You don't blog very regularly recently.
- Hmm, I don't have enough time!
- Too busy on work?
- Nope, just enjoying the fun of the youth that I have missed for years :D

But...

During that, I waste a lot of time without actually doing any useful thing, mostly in waiting time, e.g waiting to my gf to be ready (oh the girl! 😅), waiting for my friends to come for a coffee or any other kind of waiting. I started to think about blogging on the go. However, the only thing that I have in thoses cases is my smart phone, an iOS powered one. And dealing with all those jekyll and git stuffs on a smartphone is really a big challenge.

Let's make the impossible become real.

#  First obstacle: Git, of course

Coming from the terminal and Emacs world, I have never imagined how I would use git without them. But now I do :D

[Working copy](https://appsto.re/vn/xONC1.i) by Anders Borum is a quite good choice. You have the option to pay $14.99 in order to unlock the push feature. Actually, you **have to** pay. Who can use git without push feature :LOL:

For me, this's quite adequate. All the steps to clone and push from Github are set up automatically, just input your credential and done. It just took me few minutes to get used to the UI. There are Git2Go at the same price, but I feel satisfied with this so I will leave Git2Go for the next chance.

![working copy][wc]

<!-- more -->

# Markdown editor

[Byword](https://appsto.re/vn/bq7UC.i) seems to be one of the best solution out there. Almost all the articles that I found on the internet refer to this as the first option. With all the basic markdown features included, good looking font for editing and nice preview mode, it's worth to spend $3.99 on (money again, and not the last one).

![byword][bw1]

![byword][bw2]

# Text Snippets and Navigation

Back to the old days when I was still using iPhone 3g with ios 4.2.1, the text navigation and snippet experience was awful compare to Android with its 3rd party keyboards. But for now, after years coming back from Android, I'm really impressed by what iOS can do. Yes you can install 3rd party keyboards now like [Go keyboard](https://appsto.re/vn/SfiN2.i) and [Text Expander](https://appsto.re/vn/QLQR2.i). The great thing is that **Byword** integrates with Text Expander directly, thus allows you to use Text Expander with another custom keyboard. And yes, I spent another $4.99 for Text Expander.

One annoying thing that I have not found the good solution is text selection. I wish there are some ways that I can do sticky shift selection using arrow keys or swipe instead of using the default iOS selection feature.

# Code editor

Don't laugh at me and scream WTF :D My blog is a tech blog and code snippet is one essential part. At least, having something to write short code snippet with correct indentation and highlighting is still better than doing that in a plain text editor or in **Byword**. Don't tell me to run Emacs on my iPhone, I'm not that hard core :LOL:

I have tried [Textastic](https://appsto.re/vn/1LLI-.i) and [Coda](https://appsto.re/vn/5KZ2D.i), both costs $9.99 (yes I purchased both of them 🙁). However, I would say that I will go with **Coda** for now. Although it lacks Clojure schema, it has auto pair feature for parenthenses and quote. I can use Scheme syntax instead 😂. Who can code without autopair these days? 😅

![Coda][cd]

I have also tried [Lisping](https://appsto.re/vn/w0PHE.i), an editor specifically for lisp like languages, but it seems like a waste of money, very hard and inconvenience to use. Oh my $7! ☹️

# Real site preview

Okay, the last thing, just like a staging server for your blog. And of course, I have my own VPS and my domain. A simple docker image like [this](https://github.com/tmtxt/jekyll-docker-compose/blob/master/images/jekyll/Dockerfile) can help me set up a private jekyll server in minutes. And if in case you want to check server logs for build status, [Serverauditor](https://appsto.re/vn/K8AUG.i) (free on App Store) will be a great choice, which offers tons of shortcuts and gestures to make your ssh session a lot easier.

![Serverauditor][sa]

# Total Cost

* Working copy: $14.99
* Byword: $3.99
* Text Expander: $4.99
* Textastic: $9.99 (wasteful, maybe)
* Coda: $9.99
* Lisping: $7.99 (wasteful)
* iKeywi: $0.99 (wasteful)
* Serverauditor: free
* Go keyboard: free
* I have my personal VPS and domain already 😄