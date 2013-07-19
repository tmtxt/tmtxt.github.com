---
layout: post
showtn: 
title: "Jekyll Bootstrap - Recents and Related Posts section"
description: ""
category: Jekyll
thumbnail: /files/2013-01-09-jekyll-bootstrap-recents-related-posts-column/thumbnail.png
tags: [jekyll]
---
{% include JB/setup %}

Those little pieces of code are used to insert a Related Posts and Recent Posts
column to Jekyll Bootstrap site. Just put them any where you want it to display
in the post template (usually **/_includes/themes/theme-name/post.html**).

## Related Posts

{% highlight html %}
<h4>Related Posts</h4>
<ul>
  {{ "{% for post in site.related_posts limit:5 "}}%}
  <li><a href="{{ "{{ BASE_PATH "}}}}{{"{{ post.url "}}}}">{{ "{{ post.title "}}}}</a></li>
  {{ "{% endfor "}}%}
</ul>
{% endhighlight %}

<!-- more -->

## Recent Posts

{% highlight html %}
<h4>Recent Posts</h4>
<ul>
  {{ "{% for post in site.posts limit:5 "}}%}
  <li><a href="{{ "{{ BASE_PATH "}}}}{{ "{{ post.url "}}}}">{{ "{{ post.title "}}}}</a></li>
  {{ "{% endfor "}}%}
</ul>	
{% endhighlight %}

**Note**: Replace **limit:5** with the number of posts you want to display.

## Demo Image

<img src="/files/2013-01-09-jekyll-bootstrap-recents-related-posts-column/thumbnail.png"
style="border-style:solid; border-width:2px" />
