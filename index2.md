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

# Posts List

<hr/>

{% for category in site.categories %} 
  <h2 id="{{ category[0] }}-ref">{{ category[0] | join: "/" }}</h2>
  <ul>
    {% assign pages_list = category[1] %}  
    {% include JB/pages_list %}
  </ul>
{% endfor %}

#### For a full list, please visit --> [Archive page](/archive.html)

#### Subscribe to [News Feed](/atom.xml)
