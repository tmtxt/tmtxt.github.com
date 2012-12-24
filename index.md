---
layout: page
title: Welcome to the Blog of the nerds!
tagline: 
---
{% include JB/setup %}

## Articles list
<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>
