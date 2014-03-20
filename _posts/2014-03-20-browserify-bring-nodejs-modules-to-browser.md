---
layout: post
title: "Browserify - Bring Nodejs modules to browsers"
description: ""
categories: [javascript]
tags: []
---
{% include JB/setup %}

# Browserify in the command line

[Browserify](http://browserify.org/) helps you to modularize your client side
Javascript code using Nodejs `require` style. It also supports transforming
Nodejs modules on npm to browser compatible ones. You can install browserify
using npm

{% highlight console %}
$ npm install -g browserify
{% endhighlight %}

Writing code with browserify is pretty easy and familiar with Nodejs developers.
Just write your code in Nodejs require style and then let browserify handle the
rest for you. For example you have 2 files, `foo.js` and `main.js` with the
content like this

- foo.js

{% highlight js %}
module.exports = function() {
    // do something and return value here
    return 1;
};
{% endhighlight %}

- main.js

{% highlight js %}
// include foo.js here
var foo = require('./foo.js');

// you can also include nodejs/npm modules. this example includes d3-browserify installed
// using npm (npm install d3-browserify)
var d3 = require('d3-browserify');

// call the foo function
foo();  // returns 1

// call the d3
var tree = d3.layout.tree(); // create a tree layout in d3
{% endhighlight %}

<!-- more -->

There is nothing special here. The code is written in Nodejs style and is very
easy to understand for Nodejs developers. Of course, this cannot be executed in
browsers since they don't offer the `require` function. Now we will use
browserify to make it compatible with browsers environment. The easiest way is
to pack everything to a bundle with browserify

{% highlight console %}
$ browserify main.js > bundle.js
{% endhighlight %}

This command will bundle everything you need to run (foo.js, d3-browserify and main.js) to a
file named **bundle.js**. That file can be executed in browser. To use it,
simply include it using the `<script>` tag

{% highlight html %}
<script src="bundle.js"></script>
{% endhighlight %}

However, there is a problem with this command, that is it packs everything in a
single file. As a result, every output file will contain all the information
about required libraries that it needs. This makes the file size much bigger and
the browsers have to reload all those libraries each time it load the new file.
To take advantage of browser's cache functionality, you can use
**external requires** provided by browserify. Back to my previous example, now
we will transform foo.js, main.js and d3-browserify to 3 separate files. The
content of those 3 files remain unchanged.

- Expose foo.js -> foo-out.js and d3-browserify -> d3-browserify-out.js

{% highlight console %}
$ browserify -r foo.js > foo-out.js
$ browserify -r d3-browserify > d3-browserify-out.js
{% endhighlight %}

- For main.js

{% highlight console %}
$ browserify -x ./foo.js -x d3-browserify main.js > main-bundle.js
{% endhighlight %}

In the browser, you need to include all those 3 files

{% highlight html %}
<script src="foo-out.js"></script>
<script src="d3-browserify-out.js"></script>
<script src="main-bundle.js"></script>
{% endhighlight %}

Since you have expose foo.js to a module, in the browser you can also use
require

{% highlight html %}
<script language="javascript">
    var foo = require('./foo.js');
</script>
{% endhighlight %}

Later when you have another module which uses foo.js, for example **bar.js**,
browserify using -x for referencing to the old file

{% highlight console %}
$ browserify -x ./foo.js > bar-out.js
{% endhighlight %}

That's all the basic commands you need to know to work with browserify. The next
part will talk about automate browserify using Gulp.

# Using Browserify with Gulp

If you haven't used Gulp before, you should take a look at this post for some
basic tasks
[Automate Javascript development with Gulp]({%post_url 2014-03-14-automate-javascript-development-with-gulp%}).
First, you need to install **gulp-browserify** plugin

{% highlight console %}
$ npm install --save-dev gulp-browserify
{% endhighlight %}

The usage is straight forward, add a pipe which call to gulp-browserify

{% highlight js %}
gulp.task('scripts', function() {
  gulp.src(['main.js'])
    .pipe(browserify())
    .pipe(gulp.dest('./build'))
});
{% endhighlight %}

The simplest form of gulp-browserify bundles everything into one file. To make it
use the external modules, add it to the on prebundle event. Back to the example
in the last section, to use it in gulp

{% highlight js %}
gulp.task('foo', function(){
  gulp.src('foo.js')
    .pipe(browserify())
    .on('prebundle', function(bundle){
      bundle.require('./foo.js');
    })
    .pipe(gulp.dest('build'));
});

gulp.task('main', function(){
  return gulp.src(main.js)
    .pipe(browserify())
    .on('prebundle', function(bundle){
      bundle.external('./foo.js');
      bundle.external('d3-browserify');
    })
    .pipe(gulp.dest('build'));
});
{% endhighlight %}

Next, run the Gulp command to browserify. However, for the npm/nodejs modules, I
haven't found any way to generate it using gulp-browserify. You need to manually
type the command to generate them only once for the first time

{% highlight console %}
$ browserify -r d3-browserify > build/d3.js
$ gulp foo
$ gulp main
{% endhighlight %}

You can also make a gulp task for watching changes in those files and then
browserify when changes happen.

**Note**: if you are placing the your files under a sub folder of your app's
root directory(of course usually you do), you need to set the basedir in
gulp-browserify, for instance

{% highlight js %}
gulp.task('main', function(){
  return gulp.src(main.js)
    .pipe(browserify({
      basedir: './'
    }))
    .on('prebundle', function(bundle){
      bundle.external('./foo.js');
      bundle.external('d3-browserify');
    })
    .pipe(gulp.dest('build'));
});
{% endhighlight %}

Now you can run all those previous commands normally from our Nodejs app root
directory.

# Note for using with front-end libraries

Some front-end libraries such as jQuery, jQuery plugins (jQuery UI) and other
libraries that uses jQuery (like Twitter Bootstrap), my advice is that you
should not use it with browserify. For example, jQuery registers itself as the
`$` symbol or `jQuery` with the global `window` object and the other libraries
refer to this symbol by default. If you use browserify for those libraries, you
have to manually assign jQuery to the global `window.$` (or use browserify-shim)
in order for the other packages (bootstrap, jquery-ui) to recognize jQuery.

A solution for this is to use [Bower](http://bower.io/), a package manager for
the Web. Only use browserify for the libraries that only available on npm and
your own modules.
