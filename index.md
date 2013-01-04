---
layout: page
title: Where the nerds gather!
tagline: Where the awesomeness is shared!
---
{% include JB/setup %}

<div class="hero-unit">
  <h1>Welcome to my blog</h1>
  <p>A blog for the IT guys</p>
  <p><a class="btn btn-primary btn-large" href="/about.html">About me</a></p>
</div>

<div class="row">
  {% for post in site.posts limit:3 %}
  <div class="span4">
    <a href="{{ BASE_PATH }}{{ post.url }}"><h2>{{ post.title }}</h2></a>
	<hr />
	<p>{% if post.thumbnail %}
	<img src="{{ post.thumbnail }}" style="height: 280px" align="center" />
	{% else %}
	<img src="/assets/themes/tmtxt-responsive/images/no-thumnail.jpg"
  style="height: 280px" align="center" />
	{% endif %}</p>
	<p>&nbsp;</p>
	<p>
	{{ post.content | strip_html | truncatewords:20 }}
	</p>
	<p>
	<a class="btn" href="{{ BASE_PATH }}{{ post.url }}">Read more...</a>
	</p>
  </div>
  {% endfor %}
</div>

<h1>Lastest Posts</h1>
{% for post in site.posts limit:15 offset:3 %}
<hr />
<div class="row">
  <div class="span2">
    {% if post.thumbnail %}
	<img src="{{ post.thumbnail }}" align="center" />
	{% else %}
	<img src="/assets/themes/tmtxt-responsive/images/no-thumnail.jpg" align="center" />
	{% endif %}
  </div>
  <div class="span10">
    <p><a href="{{ BASE_PATH }}{{ post.url }}"><h3>{{ post.title }}</h3></a></p>
	<p>{{ post.content | strip_html | truncatewords: 40 }}
	</p>
  </div>
</div>
{% endfor %}

<p>&nbsp;</p>

#### For a full list, please visit --> [Archive page](/archive.html)

# Subscribe to News Feed

**I'm a geek** blog news feed: [News Feed](/atom.xml)
