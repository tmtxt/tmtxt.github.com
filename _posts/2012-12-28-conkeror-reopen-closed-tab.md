---
layout: post
showtn: yes
title: "Conkeror - Reopen Closed tab"
description: ""
category: conkeror
thumbnail: /files/2012-12-24-conkeror-getting-started-part-1/conkeror-main.png
tags: [conkeror, emacs, browser]
---
{% include JB/setup %}

Anyone of you who have been using Conkeror all know that Conkeror does not have
**Reopen last Closed Tab** function. How painful it is compare to the other
browsers! But we can do it by ourself by adding our own code into the
.conkerorrc file.

First, we have to create an Array to hold the closed tabs.

{% highlight javascript %}
var my_closed_buffers = new Array();
{% endhighlight %}

<!-- more -->

Next, rewrite the built-in close buffer function to make Conkeror save the
current tab URL before closing it. In the code below I save only maximum 10
closed buffers. If you want you can set it to more.

{% highlight javascript %}
//save the URL of the current buffer before closing it
interactive("my-close-and-save-current-buffer",
	"close and save the current buffer for later restore",
	function(I) {
	    if(my_closed_buffers.length==10){
		    my_closed_buffers.shift(); // remove older item to save
		    // memory, just save maximum 10 buffers
		}
		my_closed_buffers.push(I.buffer.document.URL);
		kill_buffer(I.buffer); //kill the current buffer
	});
{% endhighlight %}

Rebind the key so that when press **q**, Conkeror call our new function instead
of its built-in one.

{% highlight javascript %}
undefine_key(default_global_keymap, "q");
define_key(default_global_keymap, "q", "my-close-and-save-current-buffer");
{% endhighlight %}

Implement another function to reopen the closed buffers

{% highlight javascript %}
interactive("my-open-closed-buffer",
  "open the last closed buffer", 
  function(I){
    // check if the array length > 0
    if(my_closed_buffers.length>0){
      // load the URL in new windows
      load_url_in_new_buffer(
        my_closed_buffers[my_closed_buffers.length - 1], I.window);
      // remove the first item in the array
      my_closed_buffers.pop();
    }
  });
{% endhighlight %}

Finally, bind it to whatever key combination you want to reopen the last closed
buffer.

{% highlight javascript %}
define_key(default_global_keymap, "A-W", "my-open-closed-buffer")
{% endhighlight %}
