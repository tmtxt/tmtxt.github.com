---
layout: post
title: "Using ReactJS with Browserify and Gulp"
description: ""
categories: [javascript]
tags: []
---
{% include JB/setup %}

ReactJS uses a special syntax called JSX, not the normal JS one. Usually, when
you want to work with ReactJS JSX files, you need to transform it to a normal JS
file and then operate on that file. However, with the help of 
[Reactify](https://www.npmjs.org/package/reactify), a transform for Browserify,
you won't need to compile jsx to js files anymore, just use it directly from
your code.

For example, I have two files: **view.jsx** and **main.js**. **view.jsx**
contains the definition for React view and **main.js** is the website's main
script that loads the view through `require()`.

- **view.jsx**

{% highlight js %}
var React = require('react');

var MyView = React.createClass({
  render: function(){
    return (
      <div>
        Example
      </div>
    );
  }
});
module.exports = MyView;
{% endhighlight %}

<!-- more -->

- **main.js**

{% highlight js %}
var React = require('react');

var view = require('./view.jsx'); // need to specify the jsx extension
React.renderComponent(view(), document.getElementById('content'));
{% endhighlight %}

Next, create a Gulp task for compiling **main.js** using Browserify

{% highlight js %}
var browserify = require('browserify');
var gulp = require('gulp');
var source = require("vinyl-source-stream");
var reactify = require('reactify');

gulp.task('browserify', function(){
  var b = browserify();
  b.transform(reactify); // use the reactify transform
  b.add('./main.js');
  return b.bundle()
    .pipe(source('main.js'))
    .pipe(gulp.dest('./dist'));
});
{% endhighlight %}

After you run this task (using `gulp browserify`), the output file will be
placed in **./dist** folder.
