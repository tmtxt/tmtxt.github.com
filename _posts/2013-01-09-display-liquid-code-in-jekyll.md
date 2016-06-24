---
layout: post
showtn: 
title: "Display Liquid code in Jekyll"
description: ""
category: Jekyll
thumbnail: /files/2012-12-27-jekyll-create-a-list-of-lastest-posts/thumbnail.png
tags: [jekyll, liquid]
---


One day you might want to show some liquid code that you did in a Jekyll post,
you may encounter the problem that Jekyll server misunderstands the code and
tranform it into html to display in the generated files. Sometimes this can even
lead to some errors which make the post can not be generated into html.
Fortunately, there are solutions for it.

# 1. Using extra brackets

This solution is a bit annoying and you have to get used to those crazy curly
brackets. I really don't like it but it can be faster with short liquid code.

One example with this liquid code:

{% highlight html %}
{{ "{% for post in site.posts limit:5 "}}%}
{{ "{% endfor "}}%}
{% endhighlight %}

If you want jekyll to display it in the post content, add<b> {{ "{{" }}"</b>
(two curly brackets and a double quote)
before the liquid opening tag <b> {{ "{" }}% </b> or<b> {{ "{{" }} </b>and add
<b> {{ "}}"}} </b> (a double quote and two curly brackets) before the liquid closing tag
<b> %} </b>or<b> }} </b>

<!-- more -->

The result in the markdown should look like this

{% raw %}
|.|. "|.% for post in site.posts limit:5 ",|,|%,|
|.|. "|.% endfor ",|,|%,|
{% endraw %}

Another example with this liquid code

{% highlight html %}
{{ "{{ BASE_PATH "}}}}
{% endhighlight %}

And the result in your markdown should look similar to this

{% raw %}
|.|. "|.|. BASE_PATH ",|,|,|,|
{% endraw %}

# 2. Using raw tag

For a huge block of liquid code, just put them between {{"{%raw"}}%}
and {{"{%endraw"}}%}. However, remember to put 2 spaces in the end of
each line for jekyll to break to a new line.

# Summary

Displaying liquid code in jekyll is really an annoying task. So try to avoid it
as much as you can.
