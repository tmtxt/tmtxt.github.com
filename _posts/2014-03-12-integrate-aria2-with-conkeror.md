---
layout: post
title: "Integrate aria2 with Conkeror"
description: ""
category: conkeror
tags: [aria2, conkeror]
---
{% include JB/setup %}

In my previous post, I have demonstrated how to use aria2 as the main downloader
on Unix/Linux
([Aria2 as Default Download Manager on Unix/Linux]({%post_url 2013-11-12-aria2-as-default-download-manager%})).
However, there is still one thing missing. That is browser integration. Luckily,
Conkeror allow me to customize it very easily through the init file. To achieve,
you need to modify the `content_handler_prompt` function in
**content-handler.js** file. Add this to your .conkerorrc file for it to
overwrite the conkeror's built in one (sadly, I haven't found any better
solution).

{% highlight js %}
function content_handler_prompt (ctx) {
  var action_chosen = false;
  var can_view_internally = ctx.frame != null &&
    can_override_mime_type_for_uri(ctx.launcher.source);
  var panel;
  try {
    panel = create_info_panel(ctx.window, "download-panel",
                              [["downloading", "Downloading:", ctx.launcher.source.spec],
                               ["mime-type", "Mime type:", ctx.launcher.MIMEInfo.MIMEType]]);
    // add my own option (a for aria2)
    var action = yield ctx.window.minibuffer.read_single_character_option(
      $prompt = "Action to perform: (s: save; o: open; O: open URL; c: copy URL; a: aria2; "+
        (can_view_internally ? "i: view internally; t: view as text)" : ")"),
      $options = (can_view_internally ? ["s", "o", "O", "c", "a", "i", "t"] : ["s", "o", "O", "c", "a"]));
    switch (action) {
    case "s":
      yield content_handler_save(ctx);
      action_chosen = true;
      break;
    case "o":
      yield content_handler_open(ctx);
      action_chosen = true;
      break;
    case "O":
      yield content_handler_open_url(ctx);
      action_chosen = true;
      break;
    case "c":
      yield content_handler_copy_url(ctx);
      action_chosen = true;
      break;
    case "i":
      yield content_handler_view_internally(ctx);
      action_chosen = true;
      break;
    case "t":
      yield content_handler_view_as_text(ctx);
      action_chosen = true;
      break;
    case "a":
      yield content_handler_add_to_aria2(ctx);
      action_chosen = true;
      break;
    }
  } catch (e) {
    handle_interactive_error(ctx.window, e);
  } finally {
    if (! action_chosen)
      ctx.abort();
    if (panel)
      panel.destroy();
  }
}
{% endhighlight %}

<!-- more -->

In the function above, I have modified it to add one more option (press `a`)
for calling to `content_handler_add_to_aria2` function when the download
happens. Because of that, we need to define the `content_handler_add_to_aria2`
for sending download request to aria2.

{% highlight js %}
function content_handler_add_to_aria2(ctx) {
  var source = ctx.launcher.source.spec;

  // for aria2
  var req = Components.classes["@mozilla.org/xmlextras/xmlhttprequest;1"].createInstance();
  var data = {
    'jsonrpc':'2.0',
    'id':'qwer',
    'method':'aria2.addUri',
    'params':[[source]]
  };

  // open the request
  req.open('POST', "http://localhost:6800/jsonrpc", true);
  ctx.window.minibuffer.message("Sending download to aria2...");
  req.onreadystatechange = function (aEvt) {
    if (req.readyState === 4) {
      if(req.status === 200)
        ctx.window.minibuffer.message("Download sent to aria2 successfully");
      else
        ctx.window.minibuffer.message("Error while sending download to aria2");
    }
  };
  req.send(JSON.stringify(data));
  
}
{% endhighlight %}

This function send a JSON request to aria2 RPC listening on port 6800 (aria2's
default port) so that you need to start aria2 RPC before. Please refer to my
previous post
([Aria2 as Default Download Manager on Unix/Linux]({%post_url 2013-11-12-aria2-as-default-download-manager%}))
for instruction on how to start aria2 RPC daemon.

Now open any link that download a file, press `a` when it prompt for an option
and then the download is sent to aria2 automatically.

![Download prompt](/files/2014-03-12-integrate-aria2-with-conkeror/download.png)

![Download prompt](/files/2014-03-12-integrate-aria2-with-conkeror/success.png)

![Download prompt](/files/2014-03-12-integrate-aria2-with-conkeror/error.png)

