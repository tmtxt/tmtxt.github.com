---
layout: post
title: "Emacs as a database client"
description: ""
category: emacs
tags: [emacs]
---
{% include JB/setup %}

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

To use it, simply open an SQL buffer, `M-x` and then `sql-postgres` or
`sql-mysql` or whatever database system that you want to use. Input the server
information and password. A new SQLi buffer will be created and automatically
associated with the current SQL buffer. You can type the SQL command directly
into the SQLi buffer to execute or compose the command in the SQL buffer, select
it and then call the command `sql-send-region` (bound to **C-c C-r** by default)
to execute.

![Alt Text](/files/2014-02-26-emacs-as-a-database-client/pg.png)
