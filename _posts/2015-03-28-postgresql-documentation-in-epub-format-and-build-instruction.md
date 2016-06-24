---
layout: post
title: "PostgreSQL Documentation in Epub format"
description: ""
categories: [misc]
tags: []
thumbnail: "/files/2015-03-28-postgresql-documentation-in-epub-format-and-build-instruction/cover.png"
---


[cover]: /files/2015-03-28-postgresql-documentation-in-epub-format-and-build-instruction/cover.png
[941link]: /files/2015-03-28-postgresql-documentation-in-epub-format-and-build-instruction/postgres.epub

![Cover](/files/2015-03-28-postgresql-documentation-in-epub-format-and-build-instruction/cover-small.png)

PostgreSQL official documentation is one good resource for researching
PostgreSQL features. However, the documentation on its home page are exported to
PDF format only, which make it hard to read on other devices since it has no
text-reflowable feature. Luckily, Peter Eisentraut made a small
[commit](https://github.com/postgres/postgres/commit/ff64fd49ce91534ebbfd5774a8715b11bfc09b97)
that add epub target to the documentation build to export the epub file.

Here are the building instruction and the download links to the epub file that I
have built before

<!-- more -->

# Download links

Currently, I built only the 9.4.1 documentation since it's the currently stable
version. When the later version release, hopefully the epub file will be
published on Postgres home page. Otherwise I will update it here.

[9.4.1 Documentation Epub](/files/2015-03-28-postgresql-documentation-in-epub-format-and-build-instruction/postgres.epub)

# Building instruction

Although I have install all of the required dependencies, I still cannot build
the documentation with the simple command `make epub`. I have to use this
solution (also from Peter Eisentraut).

First, clone the postgres source from Github
[https://github.com/postgres/postgres/](https://github.com/postgres/postgres/).

Next, install some dependencies (I'm using Macports on Mac)

{% highlight cl %}
$ port install docbook-sgml-4.2 libxslt
{% endhighlight %}

If you are using Macports, remember to use GNU Make, not the default make on
Mac. Install it using Macports

{% highlight cl %}
$ port install gmake
{% endhighlight %}

Now, build the documentation (remember to change `make` to `gmake` if you are
using it from Macports).
[Source](http://postgresql.nabble.com/PostgreSQL-docs-in-ePub-format-td5739418.html).

{% highlight cl %}
$ cd postgres
$ ./configure
$ cd doc/src/sgml
$ make postgres.xml
$ xsltproc http://docbook.sourceforge.net/release/xsl/current/epub/docbook.xsl postgres.xml
$ echo 'application/epub+zip' > mimetype
$ zip -r postgres.epub META-INF OEBPS mimetype
{% endhighlight %}

The final `postgres.epub` file will be located in **doc/src/sgml**.
