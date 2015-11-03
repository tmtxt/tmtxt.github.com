---
layout: post
title: "Using Gulp with Browserify and Watchify - Update Nov 2015"
description: ""
categories: [javascript]
tags: []
thumbnail: /files/2015-06-07-gulp-with-browserify-and-watchify-updated/thumb.png
---
{% include JB/setup %}

In my previous post
[Using Gulp with Browserify and Watchify - Updated]({% post_url 2015-06-07-gulp-with-browserify-and-watchify-updated %}),
I presented a solution for setting [Gulp](http://gulpjs.com/) with
[Browserify](http://browserify.org/) and
[Watchify](https://github.com/substack/watchify) using `vinyl-source-stream`.
However, that method is no longer working as Browserify updated to version
`8.0.2`. This post will demonstrate a new updated solution that has been tested
on `Browserify 12.0.1` and `Watchify 3.6.0`.

# Structure

In my project, I will have to folder named `js` for containing all the source
.js files and another folder called `dist` for outputing all the bundles after
built.

    ├─┬ js
    │ ├─┬ page1
    │ │ ├── display.js
    │ │ └── model.js
    │ ├─┬ page2
    │ │ └── controller.js
    │ ├─┬ util
    │ │ ├── validation.js
    │ │ └── notification.js
    │ └── page1.js
    │ └── page2.js
    ├── dist
    └── gulpfile.js

<!-- more -->

The build function will bundle all the \*.js file inside `js` folder (`page1.js`
and `page2.js` in this case). Other files in the sub-folders will not be built
unless they are `require`d by `page1.js` or `page2.js`. For example

- **page1.js**

{% highlight js %}
var display = require('page1/display.js');
var model = require('page1/model.js');
var validation = require('util/validation.js');
var notification = require('util/notification.js');

// do something here
...
{% endhighlight %}

- **page2.js**

{% highlight js %}
var controller = require('page2/controller.js');
var validation = require('util/validation.js');
var notification = require('util/notification.js');

// do something here
...
{% endhighlight %}

# NPM packages

Here are all the npm packages needed for running this gulpfile

{% highlight js %}
var _ = require('lodash');
var gulp = require('gulp');
var browserify = require('browserify');
var watchify = require('watchify');
var shimify = require('browserify-shim');
var plumber = require('gulp-plumber');
var uglify = require('gulp-uglify');
var notifier = require('node-notifier');
var util = require('gulp-util');
var gulpif = require('gulp-if');
var through2 = require('through2');
{% endhighlight %}

# Browserify Config

In order to use the short path `page1/display.js` instead of the
`./page1/display.js`, we need to specify the Browserify path config in
`gulpfile.js` like this

{% highlight js %}
// browserify config
var browserifyConfig = {
  basedir: '.',
  paths: './js'
};
{% endhighlight %}

# Bundle function

Usually, the gulpfile will contain 3 tasks for building js files (`build-dev`,
`watch`, `build-prod`), so we need a bundle function to avoid repetitive tasks

{% highlight js %}
// bundle
function bundle(source, bundler, mode) {
  return gulp.src(source)
    .pipe(plumber({errorHandler: browserifyError}))
    .pipe(bundler)
    .pipe(gulpif(mode === "prod", uglify({mangle: false})))
    .pipe(gulp.dest('./dist'));
}
{% endhighlight %}

`source` is the Gulp blob, `mode` is one of the three [`dev`, `watch`, `prod`],
`bundler` will be explained in the next section.

# Create Bundler function

Now the most interesting thing is the **buildBrowserify** I mentioned in the
beginning. I will name it `createBundler` here. Please read the comment in the
code for explanation

{% highlight js %}
// caching for the next build (in watch task) instead of create new bundle
var cached = {};

// create browserify transform
function createBundler(mode) {
  var bundler = through2.obj(function(file, env, next){
    // bundle function
    var bundleFunc = function(err, res){
      file.contents = res;
      next(null, file);
    };
    var filename = file.path;

    var b;
    if(mode === "dev") {
      // debug: true for creating source map
      b = browserify(filename, _.extend(browserifyConfig, {debug: true}));
    } else if(mode === 'prod') {
      b = browserify(filename, browserifyConfig);
    } else if(mode === 'watch') {
      // for the next build of watchify, get the watchify instance out from
      // cached and build
      if(cached[file.path]) {
        cached[file.path].bundle(bundleFunc);
        return;
      }
      // create new watchify instance for the first build only
      b = browserify(filename, _.extend(browserifyConfig, {cache: {}, packageCache: {}, debug: true}));
      b.plugin(watchify);
      cached[file.path] = b; // store it in cached
    }

    // event
    b.on('error', browserifyError);
    if(mode === 'watch') {
      b.on('time', function(time){util.log(util.colors.green('Browserify'), filename, util.colors.blue('in ' + time + ' ms'));});
      b.on('update', function(){
        // on file changed, run the bundle again
        bundle(filename, createBundler('watch'), 'watch');
      });
    }

    // transform (add more if you want)
    b.transform(shimify);

    b.bundle(bundleFunc);
  });

  return bundler;
}
{% endhighlight %}

# Error handler

In the above code, if there are any error occur in the build process, the
function `browserifyError` will be triggered with an error object. Here is how
to define that function

{% highlight js %}
function browserifyError(err) {
  util.log(util.colors.red('Error: ' + err.message));
  this.end();
}
{% endhighlight %}

# Gulp tasks

Now we have everything we need to build Browserify bundles, the final thing to
do is to create some Gulp tasks for calling them

{% highlight js %}
// browserify task
gulp.task('js-dev', function(){
  return bundle('./js/*.js', createBundler("dev"), "dev");
});

gulp.task('js-prod', function(){
  return bundle('./js/*.js', createBundler("prod"), "prod");
});

gulp.task('js-watch', function(){
  return bundle('./js/*.js', createBundler("watch"), 'watch');
});
{% endhighlight %}

# Sample gulpfile

I made a sample gulpfile for this example
[here](https://github.com/tmtxt/clojure-pedigree/blob/8ad8ce8a4c1f862a3919af1a7d44305cfbd61c8e/gulpfile.js#L35),
feel free to take if you like
