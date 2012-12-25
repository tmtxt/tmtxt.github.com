---
layout: page
title: Where the nerds gather!
tagline: Where the awesomeness is shared!
---
{% include JB/setup %}
[avatar]: /site-files/index/ava.JPG

# About me
<br/>
![My avatar][avatar]  
*Full name*: Tran Xuan Truong  
*Currently*: Studying Bachelor of Information Technology at RMIT University Vietnam  
*History*: Graduated from Aptech Computer Education  

# Posts List
<ul class="posts">
{% for post in site.posts %}
<li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
{% endfor %}
</ul>