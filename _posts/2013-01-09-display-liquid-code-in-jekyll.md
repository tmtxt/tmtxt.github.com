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
Fortunately, there are solutions for it.

# Using extra brackets

This solution is a bit annoyed and you have to get used to those crazy curly
brackets. I really don't like it but it can be faster with short liquid code.

One example with this liquid code:

{% highlight html %}
{{ "{% for post in site.posts limit:5 "}}%}
{{ "{% endfor "}}%}
{% endhighlight %}

If you want jekyll to display it in the post content, add<b> {{ "{{" }}"</b>
(two curly brackets and a double quote)
before the liquid opening tag <b> {{ "{" }}% </b> or<b> {{ "{{" }} </b>and add
<b> {{ "}}"}} </b> (a double quote and two curly brakets) before the liquid closing tag
<b> %} </b>or<b> }} </b>

The result in the markdown should look like this

{% capture text %}|.|. "|.% for post in site.posts limit:5 ",|,|%,|
|.|. "|.% endfor ",|,|%,|{% endcapture %}
{% include JB/liquid_raw %}

Another instance with this liquid code

{% highlight html %}
{{ "{{ BASE_PATH "}}}}
{% endhighlight %}

And the result in your markdown should look similar to this

{% capture text %}|.|. "|.|. BASE_PATH ",|,|,|,|{% endcapture %}
{% include JB/liquid_raw %}

# Using Jekyll's liquid_raw

This is a simpler method. However, for some liquid tag like <b>{{ "{{ BASE_PATH "}}}}</b>, if you want display the liquid closing tag, you must have space
between the curly brackets.

By default, there is a directory called **JB** inside the <b>_includes</b> folder.
Inside it we have a file named <b>liquid_raw</b>. We will use it to display our
liquid code. Open it and read the comment inside that file to know how to
display the liquid code.

In short, to display some liquid tag using this way, put this inside your post
content.

{% highlight html %}
{{ "{% capture text "}}%}|.|. BASE_PATH .| .|{{ "{% endcapture "}}%}  
{{ "{% include JB/liquid_raw "}}%}
{% endhighlight %}

The symbol **|.** means the liquid openning bracket<b> { </b>and **.|** means
liquid closing bracket<b> } </b>. So if you put **.|.|.|.|** continuously,
jekyll can misunderstand and translate it into three opening brackets. Instead,
break them down by putting space between them.

### Summary

Displaying liquid code in jekyll is really an annoying task. So try to avoid it
as much as you can.
