---
layout: post
title: "Using Gulp with Browserify and Watchify - Updated"
description: ""
categories: [javascript]
tags: []
thumbnail: /files/2015-06-07-gulp-with-browserify-and-watchify-updated/thumb.png
---


> **Update**: this method is outdated again. The new solution is presented here
> [Using Gulp with Browserify and Watchify - Update Nov 2015]({% post_url 2015-11-03-using-gulp-with-browserify-and-watchify-update-nov-2015 %})

# Old method

In my previous
[2014-08-06-using-watchify-with-gulp-for-fast-browserify-build.md]({% post_url 2014-08-06-using-watchify-with-gulp-for-fast-browserify-build %}),
I have demonstrated how to use Browserify and Watchify in
Gulp to automate the build process. The steps are to
create a browserify bundle and return that bundle with a bunch of pipe inside a
gulp task like this

{% highlight js %}
var b = browserify({
  cache: {},
  packageCache: {},
  fullPaths: true
});
b = watchify(b);
b.add('./main.js');
return b.bundle()
    .pipe(uglify())
    .pipe(gulp.dest(dest));
{% endhighlight %}

We have to manually add the `main.js` file into Browserify so it will become
ugly and complex when you have multiple bundles to build, not just one
**main.js** file. It would be much better if we can do something like this,
passing the source files as a glob as we usually do with Gulp

{% highlight js %}
return gulp.src(source)
    .pipe(buildBrowserify)
    .pipe(uglify())
    .pipe(gulp.dest(dest));
{% endhighlight %}

In this post, I will illustrate how to create that `buildBrowserify` function.

<!-- more -->

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
var transform = require('vinyl-transform');
var shimify = require('browserify-shim');
var plumber = require('gulp-plumber');
var uglify = require('gulp-uglify');
var notifier = require('node-notifier');
var util = require('gulp-util');
var gulpif = require('gulp-if');
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
  var bundler = transform(function(filename){
    // create browserify instance
    var b;
    if(mode === "dev") {
      // debug: true for creating source map
      b = browserify(filename, _.extend(browserifyConfig, {debug: true}));
    } else if(mode === 'prod') {
      b = browserify(filename, browserifyConfig);
    } else if(mode === 'watch') {
      // for the next build of watchify, get the watchify instance out from
      // cached and build
      if(cached[filename]) return cached[filename].bundle();
      // create new watchify instance for the first build only
      b = watchify(browserify(filename, _.extend(browserifyConfig, watchify.args, {debug: true})));
      cached[filename] = b; // store it in cached
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

    return b.bundle();
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

I made a sample gulpfile for this example at
[https://gist.github.com/tmtxt/7e48ec7a93d591216424](https://gist.github.com/tmtxt/7e48ec7a93d591216424),
feel free to take if you like

Thank [Hung Phan](https://github.com/hung-phan) and
[Nghia Hoang](https://github.com/limdauto) for providing me the idea for this.
