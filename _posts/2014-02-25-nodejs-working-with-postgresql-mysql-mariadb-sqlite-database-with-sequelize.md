---
layout: post
title: "NodeJS - Working with PostgreSQL/MySQL/MariaDB/SQLite database using Sequelize"
description: ""
category: misc
tags: [nodejs, sequelize]
---
{% include JB/setup %}

Sequelize is an Object-Relational-Mapper, which is written entirely
in Javascript and can be used in the Node.JS environment. However, setting it up
might seem a little messy since the document does not cover everything that it
can do. In this post, I only cover some basic information as well as some tips
that you cannot find in the [Sequelize Docs](http://sequelizejs.com/docs).

# 1. Setting up Sequelize

## 1.1 Installing Sequelize

In order to use Sequelize, you need to install it along with the database engine
of your choice. For example

{% highlight console %}
$ npm install --save sequelize
$ npm install --save pg       # for postgres
$ npm install --save mysql    # for mysql
$ npm install --save sqlite3  # for sqlite
$ npm install --save mariasql # for mariasql
{% endhighlight %}

<!-- more -->

## 1.2 Connecting to Database

After installing Sequelize, it's time to test the connection to the database to
verify that Sequelize can work properly. This code is taken from Sequelize homepage.

{% highlight js %}
var Sequelize = require('sequelize')
  , sequelize = new Sequelize('database_name', 'username', 'password', {
      dialect: "postgres", // or 'sqlite', mysql', 'mariadb'
      port:    3306, // or 5432 (for postgres)
    })
 
sequelize
  .authenticate()
  .complete(function(err) {
    if (!!err) {
      console.log('Unable to connect to the database:', err)
    } else {
      console.log('Connection has been established successfully.')
    }
  })
{% endhighlight %}

# 2. Defining Database Schema

The Sequelize document describe quite clearly about how to represent your data
structure with Sequelize
[Sequelize Models](http://sequelizejs.com/docs/latest/models). You should take a
look at the document to know the basic of Sequelize. However, there
are something that they didn't cover in the document. If you want to generate
the models from your existing database, head to the next section.

## 2.1 Composite primary key

To define a composite primary key, simply put the attribute `primaryKey: true`
in each property belongs to that primary key. For example

{% highlight js %}
var myModel = sequelize.define('MyModel', { 
    item1: {
      type: Sequelize.INTEGER,
      allowNull: false,
	  primaryKey: true
    },
    item2: {
      type: Sequelize.INTEGER,
      allowNull: false,
	  primaryKey: true
    },
    item3: {
      type: Sequelize.INTEGER,
      allowNull: false,
	  primaryKey: true
    }
  });
{% endhighlight %}

## 2.2 Foreign key inside model definition

To define a reference for the current model (foreign key), add the attribute
`references` and `referencesKey` to the property of the model. For example, the
**Project** model has a property **userId** that references to the **id** in the
**User** model.

{% highlight js %}
var User = sequelize.define('User', {
	id: {/* */}
	/* other properties */
});
var Project = sequelize.define('Project', {
	userId: {
		references: "Users", // notice the plural, it's the name of the table
		referencesKey: "id"
	}
	/* other properties */
});
{% endhighlight %}

# 3. Synchronizing with Database server

## 3.1 Let Sequelize create the tables automatically

If you don't have the database structure yet, don't worry because Sequelize will
do it all for you. You only need to define the models and call
`sequelize.sync()`. You can read more here
[Database Synchronization](http://sequelizejs.com/docs/latest/models#database-synchronization).

{% highlight js %}
// create all tables... now!
sequelize.sync();
 
// force it! drop all existing tables and re-create again
sequelize.sync({force: true});
{% endhighlight %}

Sometimes, Sequelize cannot drop tables due to the foreign constraints.

{% highlight console %}
Error: ER_ROW_IS_REFERENCED: Cannot delete or update a parent row: a foreign key constraint fails
{% endhighlight %}

To fix it, instead of simply call `sequelize.sync({force: true});`, use this one

{% highlight js %}
sequelize.query('SET FOREIGN_KEY_CHECKS = 0')
.then(function(){
    return db.sync({ force: true });
})
.then(function(){
    return db.query('SET FOREIGN_KEY_CHECKS = 1')
})
.then(function(){
    console.log('Database synchronised.');
}, function(err){
    console.log(err);
});
{% endhighlight %}

## 3.2 Use with your existing database

By default, Sequelize does not provide any way for you to generate the schema
from an existing database. You have to define it manually. However,
**[sequelize-auto](https://github.com/sequelize/sequelize-auto)** can help you
**partially** fulfill this task.

The tool is really easy to use. First, you need to install it using npm

{% highlight console %}
$ npm install -g sequelize-auto
{% endhighlight %}

To generate database, execute this command. Change the arguments to your
corresponding values.

{% highlight console %}
$ sequelize-auto -o "./output-folder" -d database_name -h localhost -u username -p port -x my_password -e postgres
{% endhighlight %}

**sequelize-auto** will then generate all the models for you in the
output-folder. Later, you can use the `import` function provided by Sequelize
([Sequelize.import](http://sequelizejs.com/docs/latest/models#import)) to
populate your models. However, as I said before, **sequelize-auto** only reduces
the work that you have to do, not a complete solution so you still have to edit
the generated model files.

# 4. The rest

Again, this post just shows the basic installation, setting up steps that
Sequelize Docs does not clearly state. For other features, I advise you to read
through the [Sequelize Docs](http://sequelizejs.com/docs).
