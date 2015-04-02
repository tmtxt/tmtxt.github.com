---
layout: post
title: "Vagrant and Ansible - Organize for reusability"
description: ""
categories: [misc]
tags: []
thumbnail: "/files/2015-04-02-vagrant-and-ansible-organize-for-reusability/thumbnail.png"
---
{% include JB/setup %}

# Vagrant and Ansible

Recently, I have started a personal project built with Clojure, a website for
managing family records and visualizing pedigree tree. The problem is that I
need an automation tool for setting up the development environment and deploying
my website to the server. And yes, [Vagrant](https://www.vagrantup.com/) is one
of the best solution out there. I have used Vagrant with
[Chef](https://www.chef.io/chef/) before and found that Chef is a bit complex
and requires installation of Ruby and Chef on the server before you can
deploying anything.

After looking at all of
[Vagrant's Provisioning solution](https://docs.vagrantup.com/v2/provisioning/index.html),
I decided to give [Ansible](http://www.ansible.com/home) a try because it is
simple and operate over SSH, which means I need to install nearly nothing on the
server to use it (of course you still can Ansible on server and do a local
Provision there, but it's only one more command to type).

This blog post is the summary of my experience with Vagrant and Ansible, how I
set up my development environment and how can I re-use the code for other types
of project. The sample project can be found at
[https://github.com/tmtxt/clojure-web-skeleton](https://github.com/tmtxt/clojure-web-skeleton).
Before you come to the next part, take a look at the basic usage of Vagrant and
Ansible.

<!-- more -->

# Basic Structure

A project with Vagrant and Ansible will look like this

{% highlight html %}
├── Vagrantfile
├── ansible
│   ├── group_vars
│   │   └── all
│   ├── main.yml
│   ├── roles
│   │   ├── apt
│   │   ├── emacs
│   │   ├── git
│   │   ├── nvm
│   │   ├── oraclejdk
│   │   ├── postgres
│   │   ├── virtualenv
│   │   └── zsh
│   └── templates
│       ├── db_config.clj
│       └── system_config.clj
├── project.clj
├── src
└── static
{% endhighlight %}

Just focus on the `Vagrantfile` and the `ansible` folder, they are the main
stuff. The other files and folders are just example of a Clojure project.

- **Vagrantfile**: storing config for Vagrant and specific Ansible variables to
override when running inside Vagrant.
- **ansible**: folder for storing Ansible stuff, used for provisioning
  - **group_vars**: defining variables used for the project
  - **main.yml**: main playbook for the project
  - **roles**: a git submodule that defines various Ansible roles for setting
    sepcific service on the server. This is reusable for other projects.

# Reusable Ansible Roles

As described in the above section, there is `roles` folder which is a git
submodule for defining reusable Ansible components. You can look at my Ansible
Roles collection here on [Github](https://github.com/tmtxt/ansible-roles). Each
folder is an [Ansible role](https://docs.ansible.com/playbooks_roles.html),
which can be created using the `ansible-galaxy` command

{% highlight cl %}
ansible-galaxy init rolename
{% endhighlight %}

Each role here is used for installing and configuring specific software or
service. You can look into the roles I defined on Github for more detail. Inside
every role, we will define some default variables and then in each project,
depending on the purpose, we can override it later.

Putting all your roles into a git sub module allows you to re-use it for other
kinds of project easily. For example, for a Clojure project and a Nodejs
project, you both need to use a database server (maybe PostgreSQL), just include
the role PostgreSQL you have defined before and the database server is set up
and ready to use.

Finally, for maximum reusability, you can set up each role folder to be another
git sub module. You can find plenty of roles that other people share on
[Ansible Galaxy](https://galaxy.ansible.com/).

# Custom playbook for each project

In the structure above, in every of my project, there is a `main.yml` playbook
for defining the task that need to set up for that kind of project. For example,
in my [Clojure project](https://github.com/tmtxt/clojure-web-skeleton), the main
playbook will look similar to this

{% highlight yaml %}
---
- hosts: all
  # re-call the existing role for setting up service
  roles:
    - apt
    - user
    - git
    - postgres
    - oraclejdk
    - clojure

  # defining tasks for clojure project here
  tasks:
    # create necessary directory
    - name: create out dirs
      file: path=/log/path state=directory

    - name: generate db.clj config
      template: src=templates/db_config.clj dest={{"{{project_dir"}}}}/src/config/db.clj
{% endhighlight %}

You can override the role variable directly inside this file or in `group_vars`
(like I did in the project on Github) or `host_vars`, for example

{% highlight yaml %}
---
project_name: skeleton
project_user: "{{project_name}}"

# pass to postgresql role
db_name: "{{project_name}}"
db_user: "{{project_user}}"
db_host: localhost
db_password: somepassword
{% endhighlight %}

As you can see, when working on a new project, what I need to do is just include
the roles I need and everything is set up for me. After that, just add the tasks
specific to that kind of project and start developing.

# Provisioning in Vagrant

Now, you have the playbook ready to run, you need to plug it into Vagrant to
start provisioning. You Vagrantfile will look like this

{% highlight ruby %}
...
Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  ...

  # provision
  config.vm.provision "ansible" do |ansible|
    # link to playbook file
    ansible.playbook = "ansible/main.yml"

    # override some information when running inside Vagrant
    # this is necessary because the Vagrant VM auto map project
    # dir to /vagrant and use the "vagrant" user to run
    ansible.extra_vars = {
      # general
      project_name: "skeleton",
      project_user: project_user,
      project_dir: "/vagrant",

      # db
      db_name: project_user,
      db_user: project_user,
      db_password: "vagrant"
    }
  end

  ...
end
{% endhighlight %}

That's it, now just run `vagrant up --provision` and the automation process will
happen automatically.
