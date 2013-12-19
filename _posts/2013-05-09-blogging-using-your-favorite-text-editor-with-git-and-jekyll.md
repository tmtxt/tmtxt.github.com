---
layout: post
title: "Blogging using your favorite Text editor with Git and Jekyll - Part 2"
description: ""
category: Jekyll
thumbnail: 
showtn: yes
tags: [git, jekyll, blog]
---
{% include JB/setup %}

> Continue from my previous post [Blogging using your favorite Text editor with Git and Jekyll - Part 1]({% post_url 2013-05-08-blogging-using-your-favorite-text-editor-with-git-and-jekyll %}),
> this time, I'll demonstrate some more advanced tips to help you get used to
> blogging with Jekyll.

# Blog configuration

By default, Jekyll stores all your global site configuration in
**\_config.yml** file (can be found in the root directory of the
site). Jekyll Bootstrap has comes with some pre-defined configurations
that you can customize
like Site title, Author information,... You can also customize post URL style
using some Jekyll template variables followed the instruction here
[http://jekyllrb.com/docs/permalinks/](http://jekyllrb.com/docs/permalinks/).

One of the most important feature of a blog is the commenting system. Again,
Jekyll Bootstrap has the built-in config to help you integrate a commenting
system to your blog. Here is the example (you can see in the config file)

{% highlight yaml %}
comments :
  provider : disqus
  disqus :
    short_name : 
  livefyre :
    site_id : 
  intensedebate :
    account : 
  facebook :
    appid : 
    num_posts: 5
    width: 580
    colorscheme: lightp
{% endhighlight %}

<!-- more -->

Also, in the config file, you'll find a section about analytics. Here is a
tutorial on how to set-up Google analytics (free) for your Jekyll Bootstrap blog
[Google Analytics for Jekyll Bootstrap]({% post_url 2013-04-05-google-analytics-for-jekyll-bootstrap %}).

# HTML Content

Usually, you compose post in Jekyll with markdown mode. However, for some
special cases, when you need complex format or layout for your post or maybe you
need to embed some html code into the post content, you can change from markdown
to html. Just rename the post file extension from .md to .html and then start
writing your post content in HTML (not markdown since Jekyll will not parse
markdown data in .html file).

# Media Embedding and File Attachment

Jekyll treats all files/folders begin with \_ specially. For other
files/folders in your web directory, it just copies everything to the compiled
site so you can easily access/link to those files by relative path. For example,
you have a folder named `images` inside the root directory of your site.
Inside that folder, you have some image files (img1.jpg, img2.jpg, img3.jpg,...)
that you want to embed inside your post, simply give the relative path from
root to that file

In HTML
{% highlight html %}
<img src="/images/img1.jpg" />
<img src="/images/img2.jpg" />
<img src="/images/img3.jpg" />
{% endhighlight %}

In Markdown
{% highlight html %}
![Alt Text](/images/img1.jpg)
![Alt Text](/images/img2.jpg)
![Alt Text](/images/img3.jpg)
{% endhighlight %}

You can do the same for file attachment. It's encouraged that you organize your
files and images for better management. Don't just throw everything in the root
directory of your site.

# Summary

That's quite enough for a simple static blog. You can do even more than that
like customizing the blog's theme, setup pagination for your site, create RSS
feeds, deploy it to your custom domain,... Here are some other posts on what I
have done with Jekyll

* [Jekyll - Create a list of Latest Posts]({% post_url 2012-12-27-jekyll-create-a-list-of-lastest-posts %})
* [Jekyll - Syntax highlighting]({% post_url 2012-12-28-jekyll-bootstrap-syntax-highlighting %})
* [Jekyll Bootstrap - Create Simple Search box]({% post_url 2012-12-28-jekyll-create-simple-search-box %})
* [Thumbnail Post List for Jekyll]({% post_url 2013-01-05-thumbnail-post-list-for-jekyll-bootstrap %})
* [Display Liquid code in Jekyll]({% post_url 2013-01-09-display-liquid-code-in-jekyll %})
* [Jekyll - Recents and Related Posts section]({% post_url 2013-01-09-jekyll-bootstrap-recents-related-posts-column %})
* [Jekyll - Normalize address to avoid duplicate Disqus comment thread]({% post_url 2013-03-17-jekyll-bootstrap-nomalize-address-to-avoid-duplicate-disqus-comment-thread %})
* [Google Analytics for Jekyll Bootstrap]({% post_url 2013-04-05-google-analytics-for-jekyll-bootstrap %})
* [Setting up Custom domain for Github Pages]({% post_url 2013-04-07-setting-up-custom-domain-for-github-pages %})
* [Wordpress Read More style for Jekyll without plugin]({% post_url 2013-05-01-jekyll-read-more-feature-without-any-plugin %})
* [Jekyll Bootstrap - Tag Cloud with TagCanvas]({% post_url 2013-05-01-jekyll-tag-cloud-with-tagcanvas %})
