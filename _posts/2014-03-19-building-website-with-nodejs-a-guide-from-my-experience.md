---
layout: post
title: "Building website with Nodejs - A guide from my experience"
description: ""
categories: [javascript]
tags: []
---
{% include JB/setup %}

# 1. Introduction

Currently, I'm building a website written entirely in Javascript using NodeJS
along with PostgreSQL database server. This series of tutorials is a summary of
my experience with NodeJS, PostgreSQL as well as how I solve the problems I
encountered. In this first post, I will give an outline of which frameworks,
technologies and the list of all following tutorials in this series. The reason
why I chose those technologies will be presented in the corresponding post.

# 2. Frameworks / Technologies

Here is the list of some main frameworks, libraries and technologies that I used
for developing the website.

## 2.1 Database

- [PostgreSQL](http://www.postgresql.org/) for database system.
- [schemup](https://github.com/brendonh/schemup) for database migration.
- [Sequelize](http://sequelizejs.com/) for Object-relational mapping in Nodejs.

## 2.2 Backend

- [Nodejs](http://nodejs.org/) for backend server.
- [Express](http://expressjs.com/) for web server.
- [Passport](http://passportjs.org/) for authentication.
- [bcrypt-nodejs](https://www.npmjs.org/package/bcrypt-nodejs) for bcrypt
  encrypting.

<!-- more -->

## 2.3 Front end

- [jQuery](http://jquery.com/)
- [twitter bootstrap](http://getbootstrap.com/)
- [Browserify](http://browserify.org/) for transform Nodejs module to browser.
- [React](http://facebook.github.io/react/)
- [d3js](http://d3js.org/) for visualizing data.
- [Bower](http://bower.io/) for front-end packages manager.

## 2.4 Others

- [i18n-2](https://github.com/jeresig/i18n-node-2) for internationalization.
- [Gulp](http://gulpjs.com/) for tasks automation.
- [nginx](http://nginx.org/) for https proxy server. This can be applied for
  many other kinds of server, not just Nodejs

# 3. Articles list

- [Install and Create basic structure for Nodejs website]({%post_url 2014-03-19-install-and-create-basic-structure-for-nodejs-website%})
- [Nodejs - Express with ejs/stylus basics]({%post_url 2014-03-19-nodejs-express-with-ejsstylus-basic-explaination%})
- [Nodejs with Express - More advanced stuff]({%post_url 2014-03-19-nodejs-with-express-more-advanced-stuff%})
- [Automate Javascript development with Gulp]({%post_url 2014-03-14-automate-javascript-development-with-gulp%})
- [NodeJS - Working with PostgreSQL/MySQL/MariaDB/SQLite database using Sequelize]({%post_url 2014-02-25-nodejs-working-with-postgresql-mysql-mariadb-sqlite-database-with-sequelize%})
- **Updating...**
- For some technologies that I listed here (nginx, postgreSQL,...), you can find
  the posts about them [Archive](/archive.html) page. I don't listed them here
  because they can be used for many other types of application, not just Nodejs

<!-- zoom behavior in d3 -->

# 4. Extra for Emacs users

As I'm an Emacs user, I also wrote some articles about setting up the
development environment for Javascript on Emacs. You can visit the
[Emacs category](http://truongtx.me/categories.html#emacs-ref) on my blog to
search for those tips.
