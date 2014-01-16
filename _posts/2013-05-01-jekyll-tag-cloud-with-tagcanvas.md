---
layout: post
title: "Jekyll Bootstrap - Tag Cloud with TagCanvas"
description: ""
category: Jekyll
thumbnail: 
showtn: yes
tags: [jekyll, tag cloud, tagcanvas]
---
{% include JB/setup %}

Today, I've just added one new feature for my Jekyll blog. It's an interesting
cloud to display all the tags of my blog (you can see it in the sidebar from the
home page).

In this post, I will demonstrate how to implement
[TagCanvas](http://www.goat1000.com/tagcanvas.php) with Jekyll.

# Install TagCanvas

First, we need to download and install TagCanvas for our website. Follow the
instruction [here](http://www.goat1000.com/tagcanvas-install.php). After
downloading the javascript file, put it somewhere inside your Jekyll site.
Include the js file in the head of the template (change the file path according
to your file's path)

{% highlight js %}
<script src="{{ "{{ ASSET_PATH "}}}}/jquery/jquery-1.8.3.min.js"></script>
<script src="{{ "{{ ASSET_PATH "}}}}/tagcanvas/jquery.tagcanvas.min.js"></script>
{% endhighlight %}

# Output the tag element

In your site template, put this where you want the Tag Cloud to appear

{% highlight html %}
{% raw %}
<div id="myCanvasContainer">
  <canvas width="300" height="300" id="myCanvas">
    <p>Anything in here will be replaced on browsers that support the canvas element</p>
  </canvas>
</div>
<div id="tags">
  <ul>
	{% for tag in site.tags %}
		<li>
		  <a href="/tags.html#{{ tag | first}}-ref"
			 style="font-size: {{tag | last | size | times:100 | divided_by:site.tags.size}}pt">
			{{ tag | first }}
		  </a>
		</li>
	{% endfor %}
  </ul>
</div>
{% endraw %}
{% endhighlight %}

<!-- more -->

This code will generate a TagCanvas element and all tags belong to your Jekyll
site and also the size of each tag corresponding to its number of post.

# Start TagCanvas

The last step is to start TagCanvas when the page is loaded. Also, we need to
config **weight: true** to enable the display of tags by their weight. You can
also set the **weightSizeMin** and **weightSizeMax** if you want.

{% highlight js %}
<script type="text/javascript">
  $(document).ready(function() {
  if(!$('#myCanvas').tagcanvas({
  textColour: '#157ab5',
  outlineColour: '#ff00ff',
  reverse: true,
  depth: 0.8,
  weight: true,
  weightSizeMin: 10,
  weightSizeMax: 40,
  wheelZoom: false,
  maxSpeed: 0.05
  },'tags')) {
  // something went wrong, hide the canvas container
  $('#myCanvasContainer').hide();
  }
  });
</script>
{% endhighlight %}

{% include tag_cloud %}
