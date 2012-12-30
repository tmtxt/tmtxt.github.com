---
layout: page
title: Where the nerds gather!
tagline: Where the awesomeness is shared!
---
{% include JB/setup %}

<p align="center"><img src="/files/index/cover.png" /></p>

# Lastest Posts
<ul class="posts">
{% for post in site.posts limit:20 %}
<li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
{% endfor %}
</ul>

#### For more posts, please visit --> [Archive page](/archive.html)

# Subscribe to News Feed

**I'm a geek** blog news feed: [News Feed](/atom.xml)
