---
layout: page
title: Where the nerds gather!
tagline: Where the awesomeness is shared!
---
{% include JB/setup %}

<div class="hero-unit">
  <h1 id="unicode-item">Welcome to Trường TX blog</h1>
  <p>A blog by IT guy and for the IT guys</p>
  <p><a class="btn btn-primary btn-large" href="/about.html">About me</a></p>
</div>

<p>&nbsp;</p>

<h1>--&gt; Latest Posts &lt;--</h1>
<hr/>
{% for post in site.posts limit:5%}
<h1><a href="{{ BASE_PATH }}{{ post.url }}" class="btn btn-danger btn-large">{{ post.title }}</a></h1>
&raquo; <span><u>{{ post.date | date_to_string }}</u></span>
&raquo; {{ post.content }}
<hr/>
{% endfor %}

#### For a full list, please visit --> [Archive page](/archive.html)

#### Subscribe to [News Feed](/atom.xml)
