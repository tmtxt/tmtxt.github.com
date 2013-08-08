---
layout: post
showtn: 
title: "Conkeror - Display Tab bar"
description: ""
category: conkeror
thumbnail: /files/2012-12-27-conkeror-display-tab-bar/tab.png
tags: [conkeror, emacs, browser]
---
{% include JB/setup %}

> **Note**: for those of you who don't know what Conkeror is, please read this
> post
> [Conkeror and How it changed the way I surf the web](/2012/12/24/conkeror-and-how-it-changed-the-way-i-surf-the-web/).
> If you don't know about the conkeror rc file, please read this post
> [Conkeror Getting Started](/2012/12/25/conkeror---getting-started---part-2/)

If this is the first time you've been to Conkeror, you will find it a bit
difficult to approach. It does not have the full GUI like the other browsers.
Everytime you want to switch to the other buffer, you have to type **C-x b**,
which is very hard for you to imagine which and how many web pages are currently
opened. In this post, I'll show you how to make Conkeror display the tab bar.

![Conkeror Tab bar](/files/2012-12-27-conkeror-display-tab-bar/tab.png)

<!-- more -->

**Note**: If you Conkeror rc file, which is the **.conkerorrc** file in you Home
directory, is not a directory, make a directory call **.conkerorrc** in your
Home directory. After that move the old **.conkerorrc** file into the newly
created directory and rename that file to **init.js**. From now everything you
want to put in the **.conkerorrc** file, just put it in that **init.js** file.

# Now we can start!

In your **.conkerorrc** directory, create a folder named **themes**. Download
this file
[tommytxtruong.zip](/files/2012-12-27-conkeror-display-tab-bar/tommytxtruong.zip)
and then extract it into the **themes** directory you've just created. After
that, add this to your **.conkerorrc** file or any .js file (init.js) inside the
**.conkerorrc** directory.

{% highlight javascript %}
theme_load_paths.unshift("~/.conkerorrc/themes/");
theme_unload("default");
theme_load("tommytxtruong");
{% endhighlight %}

Now restart Conkeror to see the changes ;)

-----

**Conkeror Homepage**: <http://conkeror.org/>  
**My conkeror on github**: <https://github.com/tommytxtruong/conkerorrc>  
*Follow me and we can exchange the experience.*  

