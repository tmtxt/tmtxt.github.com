---
layout: post
title: "Working with Neo4j from Emacs"
description: ""
categories: [emacs]
tags: []
thumbnail: 
---
{% include JB/setup %}

# Neo4j and Emacs

Neo4j is one of the most powerful graph database. However, support for Neo4j in
Emacs is still limited. Luckily, with the help of comint-mode, we can easily
create a custom inferior shell for executing query and retrieving the result
from a running Neo4j instance. This
[article](https://www.masteringemacs.org/article/comint-writing-command-interpreter)
from [Mastering Emacs](https://www.masteringemacs.org/) illustrates how to write
your own command interpreter in Emacs. Based on that guide, I have developed a
new package for Emacs to simplify the steps of composing and testing cypher
query command. This post will summarize my experience and my setup to interact
with Neo4j from Emacs.

# Cypher mode

First, of course you need a major mode for displaying and editing the cypher
query command. You can easily install it using
[package.el]({% post_url 2013-01-07-emacs-package-manager %}). The mode will
automatically associate any files with `.cyp` and `.cypher` extension so you
don't need to do any thing after installing it. It also supports basic
indentation beside the must-have syntax highlighting.

![cypher-mode](/files/2015-07-04-working-with-neo4j-from-emacs/cypher.png)

<!-- more -->

# n4js.el - The Neo4j shell for Emacs

This is the package that I wrote to help simplify the steps of working with
Neo4j from Emacs. Currently, I have not pushed it to melpa (but it will be) so
you need to install it manually from here
[https://github.com/tmtxt/n4js.el](https://github.com/tmtxt/n4js.el).

By default, you need to install `cypher-mode` before you can use `n4js.el` since
it uses the font lock keywords from cypher-mode. To install it, clone it into
your `.emacs.d` and add this to your **init.el** file

{% highlight cl %}
(add-to-list 'load-path "~/.emacs.d/n4js.el/")
(require 'n4js)
{% endhighlight %}

Next, define key bindings for the functions that you want to use. The package
page on Github already listed all the functions that you can use. You may want
to define the key in cypher mode only. For example

{% highlight cl %}
(define-key cypher-mode-map (kbd "M-s-s") 'n4js-start)
(define-key cypher-mode-map (kbd "M-s-r") 'n4js-send-current-region)
(define-key cypher-mode-map (kbd "M-s-d") 'n4js-dwim)
(define-key cypher-mode-map (kbd "M-s-b") 'n4js-switch-to-buffer)
{% endhighlight %}

By default, you need to make sure that `neo4j-shell` can be located in your load
path and neo4j shell is running on the local machine with the default port
(1337) and default name (`shell`). Otherwise, you need to change the
corresponding variables to fit your need

{% highlight cl %}
(setq n4js-cli-program "/path/to/neo4j-shell")
(setq n4js-cli-arguments '("-port" "7475"))
{% endhighlight %}

You may optionally toggle truncate line feature to avoid the long row being
splitted across multiple line

{% highlight cl %}
(add-hook 'neo4j-shell-mode (lambda () (toggle-truncate-lines t)))
{% endhighlight %}

![n4js](/files/2015-07-04-working-with-neo4j-from-emacs/n4js.gif)

# Connect to a remote Neo4j instance

Usually, passing the argument `-port` and `-host` is not enough if you want to
connect to a neo4j shell that is not running on your computer because neo4j
shell requires some other port for running over RMI. In that case, you can use
the ssh tunnel for connecting

{% highlight console %}
$ ssh user@host -p 2222 /path/to/neo4j-shell -port 12345
{% endhighlight %}

To achieve that, add this to your .emacs

{% highlight cl %}
(setq n4js-cli-program "ssh")
(setq n4js-cli-arguments '("user@host" "-p" "2222" "/path/to/neo4j-shell -port 12345"))
{% endhighlight %}

To connect to a neo4j shell instance running inside Vagrant, use the
`vagrant ssh` command like this

{% highlight console %}
$ vagrant ssh -c '/path/to/neo4j-shell -port 12345'
{% endhighlight %}

To achieve that, add this to your .emacs

{% highlight cl %}
(setq n4js-cli-program "vagrant")
(setq n4js-cli-arguments '("ssh" "-c" "/path/to/neo4j-shell -port 12345"))
{% endhighlight %}

**Note**: in this case, you need to change to a dired buffer or a file inside
that project folder (or any buffer with `default-directory` inside the project
folder) so that when running the start command, it will run with the cwd is the
`default-directory`. You only need to do this once when you call the command
`n4js-start`.
