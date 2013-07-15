---
layout: post
showtn: yes
title: "Jekyll - Create a list of Latest Posts"
description: ""
category: Jekyll
thumbnail: /files/2012-12-27-jekyll-create-a-list-of-lastest-posts/thumbnail.png
tags: [jekyll]
---
{% include JB/setup %}

After a while googling for making a list of lastest posts in Jekyll, I finally
found the solution. Simply add this code to any page you want the list to
appear.

{% highlight html %}
<ul class="posts">  
	{{ "{% for post in site.posts limit:20 "}}%}  
	   <li>  
		   <span>{{ "{{ post.date | date_to_string "}}}}</span> &raquo;  
		   <a href="{{ "{{ BASE_PATH "}}}}{{ "{{ post.url "}}}}">  
		   {{ "{{ post.title "}}}}</a>  
	   </li>  
	{{ "{% endfor "}}%}  
</ul>
{% endhighlight %}

<!-- more -->

Actually, it is nearly the same with the default post list that Jekyll Bootstrap auto creates
in the index file when installing Jekyll Bootstrap. I just add the **limit:20** in the
**for** loop so that when I have many posts the list will not expand too long.
Based on this you can make your site look more professional by customizing it to
dislay the newest post in which category. ;)
