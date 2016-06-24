---
layout: post
title: "Conkeror - Working with web page permission"
description: ""
categories: [conkeror]
tags: []
thumbnail:
---


Conkeror is not a browser for everyone. It lacks many features that are waiting
for the users to implement :D One of the issue you may find annoying when
dealing with modern websites is the permission management. In other browsers,
when the web page want to access current location information or request for
camera recording, the browsers will pop up a small prompt to ask the user for
allowance. However, in Conkeror, there is no such thing. This is how to make
that possible

Currently, there are 4 kinds of permissions available, there are

- audio-capture
- video-capture
- geolocation
- desktop-notification

They are managed by the XPCOM `nsIPermissionManager` service. You need to access
that through `nsIComponentManager` like this

{% highlight js %}
const permissionManager = Components.classes["@mozilla.org/permissionmanager;1"]
        .getService(Components.interfaces.nsIPermissionManager);
{% endhighlight %}

Next, we need a function for prompting the user for which permission to enable
or disable. This function will prompt for user to select which permission that
they want to modify from the `permissionsList`

{% highlight js %}
// List of web api permission
var permissionsList = [
  {desc: "Audio Capture", value: "audio-capture"},
  {desc: "Video Capture", value: "video-capture"},
  {desc: "Geo Location", value: "geolocation"},
  {desc: "Desktop Notification", value: "desktop-notification"}
];

// read permission from minibuffer
var readPermission = function(I) {
  return I.minibuffer.read(
    $prompt = "Select permission:",
    $completer = new all_word_completer(
      $completions = permissionsList,
      $get_string = function(x) {return x.value;},
      $get_description = function(x) {return x.desc;}
    )
  );
};
{% endhighlight %}

<!-- more -->

After that, we need some interactive function for enabling and disabling
specific permission from a web page

{% highlight js %}
// add and remove permission for current page
var addPermission = function(I) {
  var perm = yield readPermission(I);
  var uri = make_uri(I.buffer.current_uri.prePath);
  var allow = Components.interfaces.nsIPermissionManager.ALLOW_ACTION;

  permissionManager.add(uri, perm, allow);

  I.minibuffer.message("Permission " + perm + " added");
};
var removePermission = function(I) {
  var perm = yield readPermission(I);
  var uri = make_uri(I.buffer.current_uri.prePath);
  var deny = Components.interfaces.nsIPermissionManager.DENY_ACTION;

  permissionManager.add(uri, perm, deny);

  I.minibuffer.message("Permission " + perm + " removed");
};

interactive("add-permission", "Add specific permission for current uri", addPermission);
interactive("remove-permission", "Remove specific permission for current uri", removePermission);
{% endhighlight %}

These two command will first prompt user for the specific permission and then
enable/disable that permission for the current web page. Now, you can try it on
sites like `messenger.com` or `slack.com`, which require `desktop-notification`
to be enabled for it to work properly.

You can check for the full file here [https://github.com/tmtxt/conkerorrc/blob/master/config/tmtxt-permission.js](https://github.com/tmtxt/conkerorrc/blob/master/config/tmtxt-permission.js )

![Permission](/files/2016-02-18-conkeror-working-with-web-page-permission/perm.png)
