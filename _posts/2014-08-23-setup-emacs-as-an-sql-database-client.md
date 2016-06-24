---
layout: post
title: "Setup Emacs as an SQL Database client - Part 1"
description: ""
categories: [emacs]
tags: []
thumbnail: /files/2014-08-06-setup-emacs-as-an-sql-database-client/no-truncate.png
---


# Introduction

Emacs's SQLi mode can be a better replacement for the default terminal database
client (psql, sqlite3,...). I prefer SQLi mode because it supports syntax
highlighting (and also Postgres keyword) and allows me to flexibly choose which
region in the buffer to send to the shell to execute.

SQLi is integrated by default in Emacs. The following interpreters are supported

* psql by PostgreSQL
* mysql by MySQL
* sqlite or sqlite3 for SQLite
* solsql by Solid
* SQL\*Plus by Oracle
* dbaccess by Informix
* isql by SyBase
* sql by Ingres
* osql by MS SQL Server
* isql by Interbase
* db2 by DB2 (IBM)
* inl by RELEX

Make sure that the client you need to use can be located inside the Emacs's
PATH. You can install **exec-path-from-shell** package using package.el for
Emacs to auto import the PATH from your default shell.

If you are using Mac OS, I have written a post about PostgreSQL installation and
configuration steps on Mac here:
[Install and Config PostgreSQL on Mac]({%post_url 2014-02-26-install-and-config-postgresql-on-mac%}).

<!-- more -->

# Basic Usage

To use it, simply open an SQL buffer, `M-x` and then `sql-postgres` or
`sql-mysql` or whatever database system that you want to use. Input the server
information and password. A new SQLi buffer will be created and automatically
associated with the current SQL buffer. You can type the SQL command directly
into the SQLi buffer to execute or compose the command in the SQL buffer, select
it and then call the command `sql-send-region` (bound to **C-c C-r** by default)
to execute or `sql-send-buffer` (bound to **C-c C-b** by default) to execute all
the buffer.

![Sql Basic](/files/2014-08-06-setup-emacs-as-an-sql-database-client/sql-basic.gif)

# Basic Config

If you have one server that you usually connect to, you can set its login
parameters as default using the variables `sql-***-login-params` where *** can
be one of the database type that you want, for example

{% highlight cl %}
(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))
{% endhighlight %}

By default, when you enter **SQLi** mode, Emacs does not automatically truncate
long lines. That can be hard for you to read the result returned from a table
with many columns

![no line truncate](/files/2014-08-06-setup-emacs-as-an-sql-database-client/no-truncate.png)  
No line truncate

To fix this, call `toggle-truncate-lines` in `sql-interactive-mode-hook`.

{% highlight cl %}
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
{% endhighlight %}

![with line truncate](/files/2014-08-06-setup-emacs-as-an-sql-database-client/truncate.gif)  
with line truncate

# Helpful key bindings

There are many helpful key bindings in SQL interactive mode. You can use the
describe function feature in Emacs (C-h f) to view the documentation of
`sql-interactive-mode`. Some important keys to remember are

- `comint-previous-matching-input-from-input` and
  `comint-next-matching-input-from-input` to traverse through the input history
- `sql-accumulate-and-indent` if you want to type a multi-line command.

# Connect to multiple server

If the default `sql-postgres-login-params`, `sql-mysql-login-params`,... do not
satisfy you, use the `sql-connection-alist` with the `sql-connect` function.
`sql-connection-alist` is an association list where the car of each
element determines the connection name and the cdr of those item indicates the
login params for that connection. For example

{% highlight cl %}
(setq sql-connection-alist
      '((server1 (sql-product 'postgres)
                  (sql-port 5432)
                  (sql-server "localhost")
                  (sql-user "user")
                  (sql-password "password")
                  (sql-database "db1"))
        (server2 (sql-product 'postgres)
                  (sql-port 5432)
                  (sql-server "localhost")
                  (sql-user "user")
                  (sql-password "password")
                  (sql-database "db2"))))
{% endhighlight %}

Now, to connect to those server, call the `sql-connect` function with the input
argument is the connection name. You can make some interactive functions for
quickly connect to those servers

{% highlight cl %}
(defun my-sql-server1 ()
  (interactive)
  (my-sql-connect 'postgres 'server1))

(defun my-sql-server2 ()
  (interactive)
  (my-sql-connect 'postgres 'server2))

(defun my-sql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))
{% endhighlight %}

![quick connect](/files/2014-08-06-setup-emacs-as-an-sql-database-client/multiple.gif)

# Encrypt password

Usually, you will put your config in .emacs under a version control system and
backup it to a remote server so
you will not want to store the password there. This
[post](http://emacs-fu.blogspot.com/2011/02/keeping-your-secrets-secret.html )
from [emacs-fu](http://emacs-fu.blogspot.com/ ) gives me the idea of storing
password in another file encrypted by GnuPG.

First, make sure you have GnuPG installed. Many Linux distros already come with
GnuPG pre-installed. On Mac OS, you can install it using **Macports**

{% highlight console %}
$ sudo port install gnupg
{% endhighlight %}

Create another file named `my-password.el.gpg`, make sure that it can be located
inside you emacs' load-path. Add your password to that file like this. Make sure
the key of this alist is the same with the key you defined before in
`sql-connection-alist` (in this case `server1` and `server2`).

{% highlight cl %}
(setq my-sql-password
      '((server1 "password1")
        (server2 "password2")))
(provide 'my-password)
{% endhighlight %}

Save the file and choose the encryption method that you want, or just skip it to
use the default symmetric encryption method.

Now, remove the password info in the `sql-connection-alist` and modify the
`my-sql-connect` function that we have defined in the previous step to load the
password from file

{% highlight cl %}
(setq sql-connection-alist
      '((server1 (sql-product 'postgres)
                  (sql-port 5432)
                  (sql-server "localhost")
                  (sql-user "user")
                  (sql-database "db1"))
        (server2 (sql-product 'postgres)
                  (sql-port 5432)
                  (sql-server "localhost")
                  (sql-user "user")
                  (sql-database "db2"))))

(defun my-sql-connect (product connection)
  ;; load the password
  (require my-password "my-password.el.gpg")

  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection my-sql-password)))))
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product product)
  (sql-connect connection))
{% endhighlight %}

When activated, the `my-sql-connect` function will prompt for the password to
decrypt the `my-password.el.gpg` file. However, it will ask for password just once and
only when you active the `my-sql-connect` function so that you will have to
enter the password just when you need to use it.

![password](/files/2014-08-06-setup-emacs-as-an-sql-database-client/password.gif)

# Extra: Fast database connecting and switching between buffers

We have all the necessary functions required for connecting to the database.
However, you still need to activate them through `M-x` or you can bind them to
some special key stroke but it will cost one key stroke for each server and will
result in too many key bindings that you need to remember. Even when you have
successfully connected to a server, switching between other buffers and sql
buffers is still very slow when you have many buffers opening. The solution is
using Emacs' `completing-read` function or using
[helm](https://github.com/emacs-helm/helm )'s equivalent `helm-comp-read`
one. I will use the helm version for demonstration in this post but you can
easily convert it the normal Emacs' `completing-read` function if you don't use
helm.

For more information about `helm`, you can read on its
[home page](https://github.com/emacs-helm/helm ) or read this
[mini tutorial](http://tuhdo.github.io/helm-intro.html ).

First, you need to define an alist for mapping the display names (completing
name) with the correct functions to connect to the database you want to execute
(in this case the `my-sql-server1` and `my-sql-server2` functions).

{% highlight cl %}
(defvar my-sql-servers-list
  '(("Server 1" my-sql-server1)
    ("Server 2" my-sql-server2))
  "Alist of server name and the function to connect")
{% endhighlight %}

Next, define a function for reading the user input using that alist for
completing

{% highlight cl %}
(defun my-sql-connect-server (func)
  "Connect to the input server using my-sql-servers-list"
  (interactive
   (helm-comp-read "Select server: " my-sql-servers-list))
  (funcall func))
{% endhighlight %}

Now, bind that function `my-sql-connect-server` to a key binding that you want.
The first time you activate this function, it will prompt for a server name and
connect to that database server. Later when you activate this function again, it
will jump directly to the SQL interactive buffer that it has created before.

You can also activate this function when you are inside an sql buffer and that
buffer will automatically be associated with the interactive session. Calling
`sql-send-region` or `sql-send-buffer` will execute the sql command directly in
the sql interactive buffer.

However, now the problem is that sometimes you want to open multiple sql
interactive buffers, not just one. A solution for this is to use prefix
argument. Come back to modify the function `my-sql-connect` like this. Add a
condition before calling `sql-connect` function.

{% highlight cl %}
(defun my-sql-connect (product connection)
  ;; load the password
  (require my-password "my-password.el.gpg")

  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection my-sql-password)))))
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product product)
  (if current-prefix-arg
      (sql-connect connection connection)
    (sql-connect connection)))
{% endhighlight %}

After evaluating the above code, every time you call `my-sql-connect-server`, it
will connect to a server or switch to an interactive session if exists. Pressing
`C-u` prefix before activating that function will always create a new
connection to the database server that you choose.

![helm](/files/2014-08-06-setup-emacs-as-an-sql-database-client/helm.gif)

# Sample Emacs config file

You can find the full code in my emacs config file on
[Github](https://github.com/tmtxt/.emacs.d/blob/master/config/tmtxt-sql.el)
