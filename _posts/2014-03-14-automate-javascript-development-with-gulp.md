---
layout: post
title: "Automate Javascript development with Gulp"
description: ""
categories: [javascript]
tags: [gulp, javascript]
---
{% include JB/setup %}

# Gulp Task runner

[Gulp](http://gulpjs.com/) is a task runner written in Javascript and run on
Nodejs (similar to [Grunt](http://gruntjs.com/)). It helps you to automate your
daily boring, repetitive and time consuming development tasks when working with
Javascript. Some examples of those tasks are compiling LESS file to CSS,
browserify modules, compiling and uglify JS files. Simply defining those tasks
in a task file and Gulp will take care of the rest for you. This post shows how
to install and config Gulp as well as the basic usage.

# Installation

You can install Gulp using Nodejs packages managing system

{% highlight console %}
$ npm install -g gulp
{% endhighlight %}

Also, you will need to install Gulp locally inside your project to in order to
load the module

{% highlight console %}
$ npm install --save-dev gulp
{% endhighlight %}

Next, install some required Gulp plugins, I will explain about these plugins
later

{% highlight console %}
$ npm install --save-dev gulp-uglify gulp-rename gulp-jshint
{% endhighlight %}

<!-- more -->

# A simple gulp file

Now you have all the dependencies installed, create a file named `gulpfile.js` in
the root directory of your project with the content like this

{% highlight js %}
var gulp = require('gulp');
var jshint = require('gulp-jshint');
var uglify = require('gulp-uglify');
var rename = require('gulp-rename');

gulp.task('default', ['lint', 'uglify', 'watch'] ,function() {
  // place code for your default task here
});

gulp.task('lint', function(){
  return gulp.src(['client/*.js'])
    .pipe(jshint())
    .pipe(jshint.reporter('default'));
});

gulp.task('uglify', ['lint'], function(){
  gulp.src(['client/*.js'])
    .pipe(uglify())
    .pipe(rename({suffix: '.min'}))
    .pipe(gulp.dest('public'));
});

gulp.task('watch', function() {
  gulp.watch(['client/*.js'], ['lint', 'uglify']);
});
{% endhighlight %}

First I will discuss about Gulp. There are only 5 main Gulp methods that you
need to know

* `task`: defines a new task
* `src`: specifies the source files
* `dest`: specifies the source folder
* `watch`: monitor file changes
* `start`: invoke a task

Next, back to the above code, first I defined a task named **default**. Just
skip it for now and I will come back to it later. The next task (**lint**) is
used for syntax checking. The syntax is very simple, just set the source files
using `gulp.src`. You can pass array of file names (can be regex) as the input. After
that, pass the function that you want to apply on those files (`jshint()`) using
`pipe`.

The third task (**uglify**) is a bit different and more complicated. It uses 2
modules **uglify** and **rename** at the same time. `pipe` is similar to
operations chaining. The source files will be uglified first, append a
suffix in file name and then saved in the destination folder (specified with
`gulp.dest()`, in this case the **public** folder). Usually, the last `pipe`
function in the pipe line indicates the output. In the previous task, the output
is the default jshint reporter while the output of this command will be export
to a directory using `gulp.dest()`. Also, I put `['lint']` in the task
definition. It's an array indicating the tasks that need to be completed before
this task can run.

The last one you can guess from its name. It monitors all the changes to the
source files, performs the **lint** and **uglify** tasks as the changes happens.
The **default** is just combination of all the other 3 tasks.

# Running Gulp

Open Terminal and change to your project's root directory. Run `gulp` with the
input argument is the task that you want to run

{% highlight console %}
$ gulp lint
[gulp] Using gulpfile /Volumes/tmtxt/Projects/easy_pedigree/gulpfile.js
[gulp] Starting 'lint'...
/Volumes/tmtxt/Projects/easy_pedigree/client/render_tree.js: line 249, col 55, Unexpected use of '++'.

1 error
[gulp] Finished 'lint-client' after 122 ms

$ gulp uglify
[gulp] Using gulpfile /Volumes/tmtxt/Projects/easy_pedigree/gulpfile.js
[gulp] Starting 'uglify'...
[gulp] Finished 'uglify' after 7.84 ms

$ gulp watch
[gulp] Using gulpfile /Volumes/tmtxt/Projects/easy_pedigree/gulpfile.js
[gulp] Starting 'watch'...
[gulp] Finished 'watch' after 7.04 ms
{% endhighlight %}

Starting Gulp with no argument will invoke the default task. As a result, these
two commands are the same

{% highlight console %}
$ gulp default
$ gulp
{% endhighlight %}

Now you have successfully created a simple gulp file. You can use
[Gulp Plugins Website](http://gulpjs.com/plugins/) to search for the plugins that
you need.

# Avoid errors when watching files

The default Gulp's watch method is really dumb. It makes Gulp quit unexpectedly
whenever an error happens while watching. For example, when there is a syntax
error in your code that causes the uglify module fail to parse the source file,
the watch process will quit eventually and you have to restart it manually. To
avoid this, use [gulp-plumber](https://www.npmjs.org/package/gulp-plumber/) (can
be install through npm) to keep the pipes working after error event. In order to
include it in your code, pipe it before the other operations like this

{% highlight js %}
var plumber = require('gulp-plumber');

gulp.task('lint', function(){
  return gulp.src(['client/*.js'])
    .pipe(plumber())
    .pipe(jshint())
    .pipe(jshint.reporter('default'));
});

gulp.task('uglify', function(){
  gulp.src(['client/*.js'])
    .pipe(plumber())
    .pipe(uglify())
    .pipe(rename({suffix: '.min'}))
    .pipe(gulp.dest('public'));
});
{% endhighlight %}
