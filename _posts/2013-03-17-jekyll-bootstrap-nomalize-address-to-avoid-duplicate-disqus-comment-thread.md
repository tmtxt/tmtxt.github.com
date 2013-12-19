---
layout: post
showtn: yes
title: "Jekyll - Normalize address to avoid duplicate Disqus comment thread"
description: ""
category: Jekyll
thumbnail: 
tags: [jekyll, disqus]
---
{% include JB/setup %}

Since Disqus is made for static site comment system, each Disqus thread is
identified by the current page URL. This leads to a problem that Disqus does not
understand that **http://my-site/link/to/post** and
**http://my-site/link/to/post/?fbxxx** refer to the same article. The second
link is caused by facebook sharing system. It automatically add the trailing to
the links that shared on facebook. As a result, those 2 links are considered as
2 separate threads and will not display probably.

The solution for this is to normalize the URL of the site to make them all
follow one convention. Because Jekyll sites are static websites, we need a
language that can run on client. This can be solved by using javascript.

<!-- more -->

Inside the **\_includes** folder under the root directory of your website, create
a new file named normalize_address.html with this content

{% highlight html %}
<script language="Javascript" type="text/javascript">
  // the standard URL should be used
  var normalized_location;
  normalized_location = "http://" + window.location.host + window.location.pathname;

  // if the current URL is not the standard URL, redirect it to standard URL
  if(window.location.toString() != normalized_location){
    window.location = normalized_location;
  }
</script>
{% endhighlight %}

The code above handles the URL, removes the unnecessary part in the URL and
redirects viewers to the new normalized URL. Next, we need to include it into our
layout so that it is auto-loaded everytime the guests view the page. Open
the file **default.html** in **\_includes/themes/your-current-theme/**
(your-current-theme is the name of your current using theme). Add this to the
header of the page

{% highlight html %}
{{"{% include normalize_address.html "}}%}
{% endhighlight %}
