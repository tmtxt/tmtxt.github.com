---
layout: post
title: "Ansible - Install and Work with Nodejs using nvm"
description: ""
categories: [misc]
tags: []
thumbnail:
---


[nvm](https://github.com/creationix/nvm) is my favorite tool for installing and
working with Nodejs. I can install several Nodejs versions on one machine for
different projects without affecting each other because **nvm** can install Node
locally (without root privilege) for each project user. However, since **nvm**
is a collection of shell functions, it can cause problems for using it with
non-interactive environments (for example in automation tools like
[Ansible](http://www.ansible.com/)).

I found some work around for it which I will present in this post. Some of them
are a bit ugly but at least they solve the problem. I'm still trying to find the
best solution and will post here when available.

# Install Node with nvm

As I mentioned before, **nvm** is a collection of shell functions, so if you call
**nvm** directly, you will receive the error saying that it cannot find the
**nvm** executable file. I tried sourcing it in `.profile` and use the Ansible's
shell module but still got the error. Finally, I came up with the solution that
is to source the **nvm** script directly everytime I need to run **nvm** using
one specified shell (bash in this case). The Ansible
tasks for installing Nodejs using **nvm** will look like this

{% highlight yaml %}
# nvm_user: the user with .nvm install

- name: install nodejs using nvm
  sudo: yes
  sudo_user: "{{nvm_user}}"
  command: bash -c '. ~/.nvm/nvm.sh; nvm install {{ nvm_node_version }}'

- name: set default node version
  sudo: yes
  sudo_user: "{{nvm_user}}"
  command: bash -c '. ~/.nvm/nvm.sh; nvm alias default {{nvm_node_version}}'
{% endhighlight %}

<!-- more -->

Here is an example [Ansible role](https://github.com/tmtxt/nvm-ansible)
 I made before for checking and installing
**nvm** and **NodeJS** to a specific user in the system. This is tested on
Ubuntu already.

# Install npm tools with Node in nvm

Ansible is shipped with npm module by default, so what you need is to specify
the search path for it to find the right `npm` executable file, either by
environment variables or by direct path

- Environment variables (you cannot use relative path `~`)

{% highlight yaml %}
- name: install node tools
  sudo: yes
  sudo_user: "{{"{{nvm_user"}}}}"
  npm: >-
    name=gulp
    global=yes
  environment:
    # can be different depending on nvm version
    PATH: "/home/{{"{{nvm_user"}}}}/.nvm/v{{"{{nvm_node_version"}}}}/bin:{{"{{ ansible_env.PATH "}}}}"
{% endhighlight %}

- Direct path

{% highlight yaml %}
- name: install node tools
  sudo: yes
  sudo_user: "{{"{{nvm_user"}}}}"
  npm: >-
    name=gulp
    executable=~/.nvm/v{{"{{nvm_node_version"}}}}/bin/npm
    global=yes
{% endhighlight %}

# Running npm tools with Node in nvm

Similary, for running executable installed by Node in nvm, we can also set the
environment variables (but you cannot use relative path in this case) or use the
direct path. You can also source **nvm** script to run the command directly

- Source **nvm** before running

{% highlight yaml %}
- name: run gulp build
  sudo: yes
  sudo_user: "{{"{{nvm_user"}}}}"
  command: bash -c '. ~/.nvm/nvm.sh; gulp compile'
  args:
    chdir: "{{"{{project_dir"}}}}"
  changed_when: false

{% endhighlight %}

- Environment variables (you cannot use relative path `~`)

{% highlight yaml %}
- name: run gulp build
  sudo: yes
  sudo_user: "{{"{{nvm_user"}}}}"
  command: gulp compile
  args:
    chdir: "{{"{{project_dir"}}}}"
  changed_when: false
  environment:
    # can be different depending on nvm version
    PATH: "/home/{{"{{nvm_user"}}}}/.nvm/v{{"{{nvm_node_version"}}}}/bin:{{"{{ ansible_env.PATH "}}}}"
{% endhighlight %}

- Direct path

{% highlight yaml %}
- name: run gulp build
  sudo: yes
  sudo_user: "{{"{{nvm_user"}}}}"
  command: /home/{{"{{nvm_user"}}}}/.nvm/v{{"{{nvm_node_version"}}}}/bin/gulp compile
  args:
    chdir: "{{"{{project_dir"}}}}"
  changed_when: false
{% endhighlight %}

# Final...

These solution are a bit ugly. I prefer configuring through environment variables because
it's the common solution for other tools, not just `nvm`. However, the problem
with it is that I cannot set the relative path in `PATH`, which makes it not
flexible when detecting the `nvm` executable files.
