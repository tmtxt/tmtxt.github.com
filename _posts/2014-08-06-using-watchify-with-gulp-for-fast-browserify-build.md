---
layout: post
title: "Using Watchify with Gulp for fast Browserify build"
description: ""
categories: [javascript]
tags: []
---


> Update: I made a new better solution for this. You can read it here
> [Using Gulp with Browserify and Watchify - Updated]({% post_url 2015-06-07-gulp-with-browserify-and-watchify-updated %})

# Using Watchify instead of gulp.watch

If you are using Browserify with Gulp, perhaps `gulp.watch` is not the best
solution when you want to watch for file changes and rebuild them as changes
happen. `gulp.watch` cannot recognize the dependencies of each Browserify bundle
so every time you save your file, Gulp have to re-compile all your Browserify
bundles.
[watchify](https://github.com/substack/watchify) is a solution for this.
Watchify will only re-compile the bundle if its dependencies change.

Using Watchify is very similar to normal Browserify. Watchify object is very
similar to a Browserify bundle so you can take the code from Browserify to
Watchify with very little change in code.

To use Watchify, take the Browserify code (you can read more in this post
[Browserify - Bring Nodejs modules to browsers]({%post_url 2014-03-20-browserify-bring-nodejs-modules-to-browser%})),
wrap it inside a Watchify object, add an on update event handler for it and you
are done.

{% highlight js %}
var browserify = require('browserify');
var source = require("vinyl-source-stream");
var watchify = require('watchify');

gulp.task('browserify', function(){
  browserifyShare();
});

function browserifyShare(){
  // you need to pass these three config option to browserify
  var b = browserify({
    cache: {},
    packageCache: {},
    fullPaths: true
  });
  b = watchify(b);
  b.on('update', function(){
    bundleShare(b);
  });
  
  b.add('./main.js');
  bundleShare(b);
}

function bundleShare(b) {
  b.bundle()
    .pipe(source('main.js'))
    .pipe(gulp.dest('./dist'));
}
{% endhighlight %}

<!-- more -->

# Watchify with other Gulp watch tasks

You will not need to activate the watch function all the time. Sometimes, you
just need to compile the file and see the result. In that case, we can modify
the above code a bit to create to different gulp tasks.

The below example demonstrate how to create 2 browserify task and attach the
watch task with normal gulp.watch task. It also takes use of livereload server
for auto web page reloading when the compilation finishes.

{% highlight js %}
var browserify = require('browserify');
var source = require("vinyl-source-stream");
var watchify = require('watchify');
var livereload = require('gulp-livereload');
var gulpif = require('gulp-if');
var watch;

gulp.task('browserify-nowatch', function(){
  watch = false;
  browserifyShare();
});

gulp.task('browserify-watch', function(){
  watch = true;
  browserifyShare();
});

function browserifyShare(){
  var b = browserify({
    cache: {},
    packageCache: {},
    fullPaths: true
  });
  
  if(watch) {
    // if watch is enable, wrap this bundle inside watchify
    b = watchify(b);
    b.on('update', function(){
      bundleShare(b);
    });
  }
  
  b.add('./main.js');
  bundleShare(b);
}

function bundleShare(b) {
  b.bundle()
    .pipe(source('share.js'))
    .pipe(gulp.dest('./dist'))
    .pipe(gulpif(watch, livereload()));
}

// define the browserify-watch as dependencies for this task
gulp.task('watch', ['browserify-watch'], function(){
  // watch other files, for example .less file
  gulp.watch('./less/*.less',
             ['compile-less']);

  // Start live reload server
  livereload.listen(35729);
});
{% endhighlight %}

![Watchify](/files/2014-08-06-using-watchify-with-gulp-for-fast-browserify-build/watchify.gif)

# Error Handling

Error handling in Watchify is similar to that in Browserify and there are
another blog post that I have written to describe to steps how to handle it
properly. You can read it here
[Error Handling while using gulp.watch]({%post_url 2014-07-15-handle-errors-while-using-gulp-watch%}).

> Update: I made a new better solution for this. You can read it here
> [Using Gulp with Browserify and Watchify - Updated]({% post_url 2015-06-07-gulp-with-browserify-and-watchify-updated %})
