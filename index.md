---
layout: page
title: Where the nerds gather!
tagline: Where the awesomeness is shared!
---
{% include JB/setup %}

<div class="hero-unit">
  <h1>Welcome Funky Nerdy blog</h1>
  <p>A blog by IT guy and for the IT guys</p>
  <p>You feel the use of computer is so boring? You feel that you are left
  behind by the technology?<br />
  What do you want for now?<br />
  Wanna break the it? Wanna change everything? Wanna be awesome? Wanna
  be different? Wanna be pro?<br />
  Let's go!</p>
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

<p>&nbsp;</p>

<h1>--&gt; Latest Posts &lt;--</h1>
<ul class="posts">  
	{% for post in site.posts limit:15 offset:3 %}  
	   <li>  
		   <span>{{ post.date | date_to_string }}</span> &raquo;  
		   <a href="{{ BASE_PATH }}{{ post.url }}">  
		   {{ post.title }}</a>  
	   </li>  
	{% endfor %}  
</ul>

#### For a full list, please visit --> [Archive page](/archive.html)

#### Subscribe to [News Feed](/atom.xml)
