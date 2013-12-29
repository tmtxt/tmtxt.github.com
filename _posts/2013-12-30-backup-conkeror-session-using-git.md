---
layout: post
title: "Backup Conkeror session using Git"
description: ""
category: conkeror
tags: [conkeror, git]
---
{% include JB/setup %}

Usually, there are many opening tabs in my Conkeror (more than 50). However,
sometimes Conkeror on MacOS got trouble when quitting which prevents it from
auto saving the session file or even worse the session file will appear empty next
time Conkeror starts. That makes me lose many opening tabs that I'm reading and
I cannot remember which tabs I need to re-open. The solution is using Git for
managing, backing up the session file.

Firstly, in order to manage the session file, we need to locate it on our
computer. On Mac OS, the Conkeror session file is stored under
**/Volumes/tmtxt/Library/Application Support/conkeror/Profiles/profile-name/sessions/**.
Create a git repository there. After that, we need a script that automatically
add all the files to staged area and commit with the message containing the
current time. The content of the script looks similar to this

{% highlight sh %}
now=$(date +"%T")

cd "~/Library/Application Support/conkeror/Profiles/profile-name/sessions/"
git add .
git commit -m "Session at $now"
{% endhighlight %}

Finally, hooking the function to run the script into Conkeror.

{% highlight js %}
add_hook("create_buffer_hook", function(buffer){
  shell_command_blind("sh /path/to/conkeror/session/backup/script");
});
{% endhighlight %}

Every time a new buffer is created, Conkeror will auto call the script to commit
it to git. If you want, you can add hook when a buffer is killed.

<!-- more -->
