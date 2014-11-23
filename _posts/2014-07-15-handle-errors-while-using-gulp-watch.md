---
layout: post
title: "Error Handling while using gulp.watch"
description: ""
categories: [javascript]
tags: []
---
{% include JB/setup %}

# Basic Error Handling with gulp.watch

One of the most annoying thing when using the gulp.watch API is that it crashes
whenever errors happen and you will have to start it again manually. Gulp system
uses Nodejs Stream for automation, so you can use the events system of Nodejs
Stream to handle error.

For example, I have the following Gulp task

{% highlight js %}
var gulp = require('gulp');
var less = require('gulp-less');
var minifyCSS = require('gulp-minify-css');

gulp.task('transform-less', function(){
  return gulp.src('./*.less')
    .pipe(less())
    .pipe(minifyCSS())
    .pipe(gulp.dest('./dist'));
});

gulp.task('watch', function(){
  gulp.watch('./*.less',
             ['transform-less']);
});
{% endhighlight %}

If one of the source .less file contains syntax error, gulp will crash while
watching these file. To handle it, simply add `on('error')` event after each
time you pipe the stream to a plugin.

<!-- more -->

{% highlight js %}
gulp.task('transform-less', function(){
  return gulp.src('./*.less')
    .pipe(less())
    .on('error', function(err){ console.log(err.message); })
    .pipe(minifyCSS())
    .on('error', function(err){ console.log(err.message); })
    .pipe(gulp.dest('./dist'));
});
{% endhighlight %}

# Error Handling with gulp.watch and Browserify

Browserify API returns a stream that can be used with Gulp and we can also use
the `on('error')` event to handle error. However, browserify stream is a bit
different since you need to explicitly call the `end()` method of that stream,
otherwise, it cannot continue the watch process. The gulp task will look similar
to this

{% highlight js %}
var gulp = require('gulp');
var browserify = require('browserify');
var source = require("vinyl-source-stream");

gulp.task('browserify', function(){
  var b = browserify();
  b.add('./main.js');
  
  return b.bundle()
    .on('error', function(err){
      console.log(err.message);
      this.end();
    })
    .pipe(source('main.out.js'))
    .pipe(gulp.dest('./dist'));
});
{% endhighlight %}

# Utilities for Error Handling

You can use the two modules `gulp-util` and `node-notifier` for colorful
error message output in the console and send error notification if notification
system is available

{% highlight js %}
var Notification = require('node-notifier');
var util = require('gulp-util');

// Standard handler
function standardHandler(err){
  // Notification
  var notifier = Notification();
  notifier.notify({ message: 'Error: ' + err.message });
  // Log to console
  util.log(util.colors.red('Error'), err.message);
}

// Handler for browserify
function browserifyHandler(err){
  standardHandler(err);
  this.end();
}
{% endhighlight %}

And then in the `on('error')` event of the stream, pass the handler as the
callback

{% highlight js %}
// standard handler
.on('error', standardHandler)

// or browserify handler
.on('error', browserifyHandler)
{% endhighlight %}

**Note**: on Mac, for the notification to work properly, you need to have an OSX
version that supports Notification center and
[terminal-notifier](https://github.com/alloy/terminal-notifier) installed.
