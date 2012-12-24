---
layout: page
title: Welcome to the Blog of the nerds!
tagline: Where the awesomeness is shared!
---
{% include JB/setup %}
[avatar]: /images/index/ava.JPG

## About me
<br/>
![My avatar][avatar]
  
*Currently*: Studying Bachelor of Information Technology at RMIT University Vietnam  
*History*: Graduated Aptech Computer Education  
  
## Articles list
<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>
