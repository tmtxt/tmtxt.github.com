---
layout: post
title: "Mozrepl in combination with Emacs for developing Conkeror"
description: ""
category: conkeror
thumbnail: 
showtn: no
tags: [mozrepl, emacs, mozilla]
---
{% include JB/setup %}

Conkeror and Emacs are my two most used applications, one for web browsing and
one for text editing, developing. It would be very great if I could use Emacs
to interact directly with Conkeror for developing purpose. Fortunately, I found
Mozrepl, an extension that *"lets us program Firefox and other Mozilla-based
applications from the inside"* (quoted from Mozrepl homepage). This article is
to summarize the steps on to set up Mozrepl in Conkeror and Emacs.

# Install Mozrepl extension for Conkeror

You cannot directly install Mozrepl for Conkeror from Mozilla Add-ons page.
Instead, you need to download it from the
[Mozilla Add-ons page](https://addons.mozilla.org/en-US/firefox/addon/mozrepl/),
edit the install.rdf file and do a forced-installation following the instruction
here [Conkeror Extensions](http://conkeror.org/Extensions).

# Config Mozrepl

Create a file named mozrepl-conkeror.js inside your home directory. Place this
code as the file's content

	var conkeror = Cc["@conkeror.mozdev.org/application;1"]
		.getService().wrappedJSObject;

	this.enter(conkeror);

Next, open your .conkerorrc and add this piece of code inside it to start the
Mozrepl immediately when Conkeror start and then load the config file above.

<!-- more -->

{% highlight js %}
user_pref('extensions.mozrepl.autoStart', true);
let (mozrepl_init = get_home_directory()) {
  mozrepl_init.appendRelativePath("mozrepl-conkeror.js");
  session_pref('extensions.mozrepl.initUrl', make_uri(mozrepl_init).spec);
}
{% endhighlight %}

Restart Conkeror and Mozrepl server is now ready for running.

# Emacs integration

Extract the .xpi file you've downloaded before and copy **moz.el** file to
somewhere in your Emacs' load-path. Alternatively, you can download **moz.el**
from [here](https://raw.github.com/bard/mozrepl/master/chrome/content/moz.el).

Add this to your .emacs

{% highlight cl %}
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))
{% endhighlight %}

Restart Emacs for the code to take effect. Now in every javascript file, press
**C-c C-s** to open the Mozrepl buffer. Enter the code there and press Return to
evaluate it in Conkeror. For other key bindings, see here
[https://github.com/bard/mozrepl/wiki/Emacs-integration](https://github.com/bard/mozrepl/wiki/Emacs-integration).

![Mozrepl](/files/2013-10-01-mozrepl-in-combination-with-emacs-for-developing-mozilla-based-applications/mozrepl.png)
