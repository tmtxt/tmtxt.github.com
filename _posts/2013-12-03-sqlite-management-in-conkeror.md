---
layout: post
title: "SQLite Management inside Conkeror"
description: ""
category: conkeror
thumbnail: 
showtn: no
tags: [sqlite, conkeror]
---
{% include JB/setup %}

**SQLite Manager** is a powerful extension for Firefox, which provides the ability to
manage SQLite database from within Firefox. Also, Conkeror/Firefox store their data
(history pages, bookmarks, downloads,...) using SQLite database. However, Conkeror doesn't
have any feature to have you manage its data (bookmarks, history, downloads..). You need
to query that data manually from the SQLite database file. This blog post will
demonstrate how to install and use **SQLite Manager** to manage those kinds of
data from within Conkeror.

First, you need to install the **SQLite Manager** add-on from Mozilla extensions
page. Open Conkeror and follow this url
[https://addons.mozilla.org/en-US/firefox/addon/sqlite-manager/](https://addons.mozilla.org/en-US/firefox/addon/sqlite-manager/). In my case,
everything I need to do is to click on the Download button and the extension
installer will do the rest. However, if you encounter any problem, take a look
at how to forced install the extension on Conkeror's page
[http://conkeror.org/Extensions](http://conkeror.org/Extensions).

<!-- more -->

Next, open your .conkerorrc file and add this interactive command to activate
**SQLite Manager**.

{% highlight js %}
interactive("sqlite-manager",
    "Open SQLite Manager window.",
    function (I) {
        make_chrome_window('chrome://SQLiteManager/content/sqlitemanager.xul');
    });
{% endhighlight %}

Restart Conkeror for the changes to take effect. Now type **M-x** and then
**sqlite-manager**, the **SQLite Manager** window will appear and you can open
any sqlite database to query on. You can also open Conkeror's databases which
contain information about history, downloads, bookmarks,... in the
**(Select Profile Database)** combo box on the top of the window.

![Demo](/files/2013-12-03-sqlite-management-in-conkeror/demo1.png)

---

![Demo](/files/2013-12-03-sqlite-management-in-conkeror/demo2.png)

---

![Demo](/files/2013-12-03-sqlite-management-in-conkeror/demo3.png)
