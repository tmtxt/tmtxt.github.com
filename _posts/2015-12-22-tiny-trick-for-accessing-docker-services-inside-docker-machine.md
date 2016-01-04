---
layout: post
title: "Tiny trick for accessing Docker services inside Docker Machine"
description: ""
categories: [misc]
tags: []
thumbnail: 
---
{% include JB/setup %}

Recently, I have been working with projects using docker on Mac OS using docker
machine. However, docker machine currently does not support fixed ip address for
the machine so that everytime the virtual machine boots up, it is assigned with
a new ip address. That makes accessing the docker containers running inside the
machine a bit annoying since I have to use `docker-machine ip` command everytime
to retrieve the ip of that machine and connect using the ip like
`http://192.168.1.100:8888`.

One simple solution for this is to define a fixed host name with the ip in the
**hosts** file. This little shell scripts utilizes `sed` and `tee` to
dynamically update the host name and ip of the docker machine everytime you boot
up that vitual machine.

{% highlight sh %}
#! /usr/bin/env sh

# remove the old ip in hosts file
sudo sed -i "/\b\(hostname\)\b/d" /etc/hosts

# insert the new ip
echo "$(docker-machine ip machine-name) hostname" | sudo tee -a /etc/hosts

# set env variables
eval "$(docker-machine env machine-name)" OR $(docker-machine env machine-name)
{% endhighlight %}

You will need to replace `hostname` with the server name you want to assign to
that docker machine and replace `machine-name` with the name of the docker
machine.

This script will first find and remove the old entry that containing
`hostname` in the hosts file. Next, it will append a new entry to the hosts file
by evaluating the `docker-machine ip` command to get the new ip. Finally, updates
all the environment variables for the current session for the `docker`
and `docker-compose` to work properly. Keep in mind that you need to run this
script using `source` for the `docker-machine env` command to take effect for
the current shell.

<!-- more -->

For now, everytime you boot up your docker machine, what you need to do is just
to source that shell script and you can start using all the docker commands

{% highlight console %}
$ . update-docker
{% endhighlight %}

You can also access all the services running inside that machine using the host
name that you defined before, for example

{% highlight sh %}
http://hostname:8888
{% endhighlight %}
