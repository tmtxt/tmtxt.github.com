---
layout: post
showtn: yes
title: "Jekyll Bootstrap - Create Simple Search box"
description: ""
category: Jekyll
thumbnail: /files/2012-12-28-jekyll-create-simple-search-box/thumbnail.jpg
tags: [jekyll, search]
---
{% include JB/setup %}

Jekyll is great! However, it lacks one important thing compare to the other blogging
systems. It's the search function. I've implemented a simple Google
search box in my jekyll blog. Below are the steps.

# Implement a Search box

First, create a file named **my_google_search.html** in the
**\_includes** folder in your jekyll website directory. Add this
code to the newly created html file

<!-- more -->

{% highlight html %}
<script language="Javascript" type="text/javascript">
  function my_search_google()
  {
    var query = document.getElementById("my-google-search").value;
    window.open("http://google.com/search?q=" + query
	+ "%20site:" + "http://yoursite.com");
  }
</script>
{% endhighlight %}

Replace **http://yoursite.com** with the URL of your site.

Next, open up your template file (usually
/_includes/themes/theme-name/default.html), add this line in the head area.

{% highlight html %}
<!-- my custom google search -->
{{ "{% include my_google_search.html "}}%}
{% endhighlight %}

Finally, add a form where you want the search box to appear

{% highlight html %}
<!-- my custom google search -->
<form onsubmit="my_search_google()" >
  <input type="text" id="my-google-search">
</form>
{% endhighlight %}

Ok, now you're done ;)

# More

Actually this is a very simple Google search box that can be applied for any
site not just Jekyll. Simply create a form in your page and include the script
in the js file and Google will do the rest for you.
