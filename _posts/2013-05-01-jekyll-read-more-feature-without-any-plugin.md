---
layout: post
showtn: yes
title: "Jekyll - Read More without plugin"
description: ""
category: Jekyll
thumbnail: 
tags: [jekyll]
---
{% include JB/setup %}

Jekyll does not offer Preview post feature by default. There are plenty of
plugins that can help us achieve that task. However, since many Jekyll sites are
hosted on Github and Github prohibits us to run plugins (maybe for security
reason), we may have to deploy Jekyll site on our own server or
generate the site locally and then push it onto Github. Today, after reading some
Liquid documentation, I have found a solution that can help us show the preview
of the post using only the built-in Jekyll Liquid tag with **split** and
**first** filter.

# Add separator for post

First, open a post and add this line where you want to split it. The text before
it will be converted to html (if in markdown) and then show in the preview
section.

{% highlight html %}
<!-- more -->
{% endhighlight %}

# Display the Preview

Next, open your index.html file or the page which is currently display your post
list. Find the output liquid tag {% raw %}{{ post.content }}{% endraw %}. Now we
need to add two filters that I mentioned before to the tag.

{% highlight html %}
{% raw %}
<div class="post-content-truncate">
  {% if post.content contains "<!-- more -->" %}
    {{ post.content | split:"<!-- more -->" | first % }}
  {% else %}
    {{ post.content | strip_html | truncatewords:100 }}
  {% endif %}
</div>
{% endraw %}
{% endhighlight %}

<!-- more -->

The code above will check if the post content contains the the separator string,
it will display the part before that string. Otherwise, it will strip all html
from post content and then truncate to 100 words to display the preview.

# Fix the CSS

You may notice one problem that if the preview of the post contains some special
elements like h1, h2, h3,... It can be difficult for audience to distinguish
between the current page element and the post content since they all use the
same css, all headings will look the same. Because of that, you may want to
change the css a bit. For example:

{% highlight css %}
.post-content-truncate h1 {
	font-size: 33.75px;
	color: #333;
}

.post-content-truncate h2 {
	font-size: 26.25px;
	color: #333;
}

.post-content-truncate h3 {
	font-size: 18.75px;
	color: #333;
}

.post-content-truncate img {
	max-width: 400px;
}
{% endhighlight %}
