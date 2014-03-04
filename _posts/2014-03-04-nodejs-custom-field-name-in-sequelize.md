---
layout: post
title: "NodeJS - Custom field name in Sequelize"
description: ""
category: misc
tags: [nodejs, sequelize]
---
{% include JB/setup %}

By default, [Sequelize](http://sequelizejs.com/) (an ORM system for NodeJS) only
allows you to specify custom table name. Currently, there is no such feature in
Sequelize to help you define custom column names and then map them to properties
in model objects. To achieve this, you may need a workaround that is to use
getter and setter methods provided by Sequelize. When you creating the model,
set the property names to the same with the corresponding column name in
database first. After that, define the setter and getter functions to map those
column to the desired properties name that you want. This example transforms
model properties name from camel_case (SQL style) to snakeCase (JS style).

{% highlight js %}
var model =
	sequelize.define('People', { 
    name: Sequelize.INTEGER,
    birth_date: Sequelize.DATE, // should be the same with
    death_date: Sequelize.DATE  // column name in database
  }, {
    // getters and setters
    getterMethods: {
      birthDate: function(){return this.getDataValue("birth_date");},
      deathDate: function(){return this.getDataValue("death_date");}
    },
    setterMethods: {
      birthDate: function(v){this.setDataValue("birth_date", v);},
      deathDate: function(v){this.setDataValue("death_date", v);}
    },
    timestamps: false,
    tableName: "people"
	});
{% endhighlight %}

Later, when you want to create a new instance, you can use the `birthDate`
instead of `birth_date`.

<!-- more -->

{% highlight js %}
// build the instance and set the properties
var person = model.build({
  name: "Some name",
  birthDate: new Date(),
  deathDate: new Date()
});

// retrieve the data
person.birthDate; // returns birth_date of person
person.deathDate; // returns death_date of person
{% endhighlight %}

As you can see, this solution still has one drawback, that is the the two
properties exists at the same time inside the `People` model. But at least, this
works! Let's wait for Sequelize to add this functionality in the future.
