---
layout: post
title: "Display Liquid code in Jekyll"
description: ""
category: 
thumbnail: /files/2012-12-27-jekyll-create-a-list-of-lastest-posts/thumbnail.png
tags: [jekyll, liquid, tutorial]
---
{% include JB/setup %}

One day you might want to show some liquid code that you did in a Jekyll post,
you may encounter the problem that Jekyll server misunderstands the code and
tranform it into html to display in the generated file. Sometimes this can even
lead to some errors which make the post can not be generated to html.
Fortunately, there is a solution for it. However, it's a bit annoyed and you
have to get used to those crazy curly brackets. I really don't like it but I
still have to use it until I find a better solution

One example with this liquid code:

{% highlight html %}
{{ "{% for post in site.posts limit:5 "}}%}
{{ "{% endfor "}}%}
{% endhighlight %}

or this

{% highlight html %}
{{ "{{ BASE_PATH "}}}}
{% endhighlight %}

If you want jekyll to display it in the post content, add<b> {{ "{{" }}"</b>
(two curly brackets and a double quote)
before the liquid opening tag <b> {{ "{" }}% </b> or<b> {{ "{{" }} </b>and add
<b> {{ "}}"}} </b> (a double quote and two curly brakets) before the liquid closing tag
<b> %} </b>or<b> }} </b>

The result in the markdown should look like this

<img src="/files/2013-01-09-display-liquid-code-in-jekyll/curly.png"
style="border-style:solid; border-width:2px" />
