---
layout: post
title: "Using ES6 Javascript Generator in ES5 supported platforms with Regenerator"
description: ""
categories: [javascript]
tags: []
---


One of the most important feature introduced in Javascript ES6 is the
[Generator function](http://jlongster.com/2012/10/05/javascript-yield.html),
which provides the ability to suspend the execution of a function. However, not
all browsers and platforms support generator right now. Earlier versions of
Firefox either had ES6 generators turned off, or supported only old style
generators. On Chrome 28, you have to turn on an experimental flag named
**Enable Experimental JavaScript**. Node.js haven't got generator support until
version 0.11.2 and you have to run it with `--harmony` flag.

You have another option that is to compile the javascript files using
[Regenerator](http://facebook.github.io/regenerator/), an ES6 to ES5 Javascript
compiler (Google also has their own compiler called
[Traceur](https://github.com/google/traceur-compiler)). Regenerator is available
on npm

{% highlight console %}
$ npm install -g regenerator
{% endhighlight %}

The command to transform js file is simple. You need to include the
[runtime](https://github.com/facebook/regenerator/tree/master/runtime) file in
your result html file.

{% highlight console %}
$ regenerator es6.js > es5.js
{% endhighlight %}

If you want Regenerator to include the runtime automatically for you in all
files that it generates, use `--include-runtime` flag

{% highlight console %}
$ regenerator --include-runtime es6.js > es5.js
{% endhighlight %}

<!-- more -->

If you want to use Regenerator with [Gulp](http://gulpjs.com/) (read more at
[Automate Javascript development with Gulp]({%post_url 2014-03-14-automate-javascript-development-with-gulp%})),
you need to install [gulp-regenerator](https://www.npmjs.org/package/gulp-regenerator).

{% highlight console %}
$ npm install --save-dev gulp-regenerator
{% endhighlight %}

To use it, simply pipe it to a gulp task

{% highlight js %}
var gulp = require('gulp');
var regenerator = require('gulp-regenerator');

gulp.task('default', function () {
    gulp.src('src/app.js')
        .pipe(regenerator())
        .pipe(gulp.dest('dist'));
});
{% endhighlight %}

You can also include the runtime with **options.includeRuntime**

{% highlight js %}
var gulp = require('gulp');
var regenerator = require('gulp-regenerator');

gulp.task('default', function () {
    gulp.src('src/app.js')
        .pipe(regenerator({includeRuntime: true}))
        .pipe(gulp.dest('dist'));
});
{% endhighlight %}

Another example for a gulp task that pipes multiple actions

{% highlight js %}
var gulp = require('gulp');
var regenerator = require('gulp-regenerator');
var rename = require('gulp-rename');
var uglify = require('gulp-uglify');

gulp.task('default', function () {
    gulp.src('src/app.js')
        .pipe(regenerator({includeRuntime: true}))
        .pipe(uglify())
        .pipe(rename({suffix: '.min'}))
        .pipe(gulp.dest('dist'));
});
{% endhighlight %}
