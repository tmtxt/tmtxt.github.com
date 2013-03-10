---
layout: page
title: Where the nerds gather!
tagline: Where the awesomeness is shared!
---
{% include JB/setup %}

<div class="hero-unit">
  <h1>Welcome Nerdy blog</h1>
  <p>A blog by IT guy and for the IT guys</p>
  <p>You feel the use of computer is so boring? You feel that you are left
  behind by the technology?<br />
  What do you want for now?<br />
  Wanna break the it? Wanna change everything? Wanna be awesome? Wanna
  be different? Wanna be pro?<br />
  Let's go!</p>
  <p><a class="btn btn-primary btn-large" href="/about.html">About me</a></p>
</div>

<p>&nbsp;</p>

<h1>--&gt; Latest Posts &lt;--</h1>
<ul class="posts">  
	{% for post in site.posts limit:15%}  
	   <li>  
		   <span>{{ post.date | date_to_string }}</span> &raquo;  
		   <a href="{{ BASE_PATH }}{{ post.url }}">  
		   {{ post.title }}</a>  
	   </li>  
	{% endfor %}  
</ul>

#### For a full list, please visit --> [Archive page](/archive.html)

#### Subscribe to [News Feed](/atom.xml)
