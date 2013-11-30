---
layout: post
title: "Conkeror Extended Facebook Mode - Update 21 Nov 2013"
description: ""
category: conkeror
thumbnail: 
showtn: no
tags: [conkeror, cefm]
---
{% include JB/setup %}

A few days ago, Facebook made some minor changes to its News feed that make my
CEFM mode sometimes cannot find the selected story link to open. Today, I have
released a new version of CEFM to fix that error. Also, in this update, I have
improved some existing functions and add some more features so that it can
interact more with the Facebook page.

If you are installing CEFM for first, just ignore this post and head to the
project's home page at [CEFM Homepage](/conkeror-extended-facebook-mode.html).
If you upgrade from a version before 21 Nov 2013, please consider reading these
post because I have changed some functions' names.

# Remove pre-keybindings for some Quick access commands

Before that, CEFM bound **3**, **4**, **5** to the 3 commands
**cefm-open-friend-request**, **cefm-open-messages** and
**cefm-open-notification** respectively. However, in this new release, I have
remove those key bindings to give you freedom of binding to whatever you want.

{% highlight js %}
define_key(facebook_keymap, "3", "cefm-open-friend-request");
define_key(facebook_keymap, "4", "cefm-open-messages");
define_key(facebook_keymap, "5", "cefm-open-notification");
{% endhighlight %}

# Change Commands Prefix

Since this release, I have changed the prefix of all commans from facebook- to
cefm-. That is to prevent duplicate commands if Conkeror includes some commands
for Facebook with the same name. You need to change your key bindings in order
to use the new version. Here is the list of changes

<!-- more -->

- **facebook-open-current-story-new-buffer** -> **cefm-open-current-story-new-buffer**
- **facebook-open-current-story-new-buffer-background** -> **cefm-open-current-story-new-buffer-background**
- **facebook-cycle-conversations** -> **cefm-cycle-conversations**
- **facebook-scroll-up-current-conversation** -> **cefm-scroll-up-current-conversation**
- **facebook-scroll-down-current-conversation** -> **cefm-scroll-down-current-conversation**

# Add new commands

In this new release, I have included 3 more commands for faster interaction
with Facebook. They are **cefm-expand-content**, **cefm-quick-logout** and
**cefm-open-home**. Information about them can be found on the
[CEFM Homepage](/conkeror-extended-facebook-mode.html).
