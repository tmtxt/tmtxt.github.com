---
layout: post
showtn: yes
title: "Conkeror - Get tinyurl for the current page"
description: ""
category: conkeror
thumbnail: /files/2012-12-30-conkeror---get-tinyurl-for-the-current-page/tinyurl.png
tags: [conkeror, tinyurl]
---
{% include JB/setup %}

The little code below helps us to quickly get tinyurl link for the currently viewed
page instead of having to open tinyurl webpage, copy paste the link and get the
new link. Put it in your .conkerorrc

{% highlight javascript %}
// get tiny url for the current page
// press * q and then c to generate and copy the tinyurl into clipboard
define_browser_object_class(
    "tinyurl", "Get a tinyurl for the current page",
    function (I, prompt) {
        check_buffer(I.buffer, content_buffer);
        let createurl = 'http://tinyurl.com/api-create.php?url=' +
            encodeURIComponent(
                load_spec_uri_string(
                    load_spec(I.buffer.top_frame)));
        try {
            var content = yield send_http_request(
                load_spec({uri: createurl}));
            yield co_return(content.responseText);
        } catch (e) { }
    });
define_key(content_buffer_normal_keymap, "* q", "browser-object-tinyurl");
{% endhighlight %}

<!-- more -->

To get the tinyurl of the current page, simply press <span><b>* q c</b></span>,
the generated tinyurl now is copied to your clipboard.
