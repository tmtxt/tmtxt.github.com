---
layout: post
title: "Using Bower with Gulp for Automatic Frontend Libraries Installation"
description: ""
categories: [javascript]
tags: []
---
{% include JB/setup %}

# 1 .Bower

[Bower](http://bower.io/) is the most popular front-end package manager. You can
install most of the front-end libraries from Bower. However, the biggest
weakness of bower is that its functionality is just like a downloader. Each
Bower package has a different structure and it is not a very good solution for
us when we have to manually include the script files for each new library that
we install. In this post, I will demonstrate the solution for that problem. This
is not a fully automated method, only for the js part, but that still saves you
a lot of time. Also, all the solutions presented in this post are defined in Gulp.

# 2. Install Bower packages with Gulp

Before processing to this part, make sure that you have Bower and Gulp command
line installed already. You may need a **.bowerrc** file in your project root
directory (or the directory that you want bower to run from), but this is
optional. If the **.bowerrc** file is not presented, the default setting will be
loaded. You can read more information about Bower configuration
[here](http://bower.io/docs/config/).

Now we will create a Gulp task for installing the libraries describes in
**bower.json** using the configuration in **.bowerrc** automatically.

{% highlight js %}
var gulp = require('gulp');
var bower = require('bower');

gulp.task('bower', function(cb){
  bower.commands.install([], {save: true}, {})
    .on('end', function(installed){
      cb(); // notify gulp that this task is finished
    });
});
{% endhighlight %}

<!-- more -->

If you are install the library for the first time, use bower command line to
write the dependencies into the **bower.json** file

{% highlight console %}
$ bower install --save library_name
{% endhighlight %}

Later, when the other developers fetch the new code to their local computers,
they can run the **bower** task

{% highlight console %}
$ gulp bower
{% endhighlight %}

Actually, you can just run `bower install` to install all packages listed in
**bower.json**. However, the advantage of using Gulp is that you can chain the
tasks together, specify which task should run after finishing installing bower
packages.

# 3. Auto bundle Bower libraries

Each Bower library has its own **bower.json** file, which specifies the
dependencies of that library. We will need a Gulp task that parse these files
and add them to a new file in the right order (dependencies first and then the
library itself after). The explanation is in the next part.

{% highlight js %}
var underscore = require('underscore');
var underscoreStr = require('underscore.string');
var concat = require('gulp-concat');
var gulp = require('gulp');

var exclude = ['lib1', 'lib2'];

gulp.task('bundle-libraries-auto', ['bower'], function(){
  var bowerFile = require('./bower.json');
  var bowerPackages = bowerFile.dependencies;
  var bowerDir = './bower_components';
  var packagesOrder = [];
  var mainFiles = [];

  // Function for adding package name into packagesOrder array in the right order
  function addPackage(name){
    // package info and dependencies
    var info = require(bowerDir + '/' + name + '/bower.json');
    var dependencies = info.dependencies;
    
    // add dependencies by repeat the step
    if(!!dependencies){
      underscore.each(dependencies, function(value, key){
        if(exclude.indexOf(key) === -1){
          addPackage(key);
        }
      });
    }
    
    // and then add this package into the packagesOrder array if they are not exist yet
    if(packagesOrder.indexOf(name) === -1){
      packagesOrder.push(name);
    }
  }

  // calculate the order of packages
  underscore.each(bowerPackages, function(value, key){
    if(exclude.indexOf(key) === -1){ // add to packagesOrder if it's not in exclude
      addPackage(key);
    }
  });

  // get the main files of packages base on the order
  underscore.each(packagesOrder, function(bowerPackage){
    var info = require(bowerDir + '/' + bowerPackage + '/bower.json');
    var main = info.main;
    var mainFile = main;

    // get only the .js file if mainFile is an array
    if(underscore.isArray(main)){
      underscore.each(main, function(file){
        if(underscoreStr.endsWith(file, '.js')){
          mainFile = file;
        }
      });
    }

    // make the full path
    mainFile = bowerDir + '/' + bowerPackage + '/' + mainFile;

    // only add the main file if it's a js file
    if(underscoreStr.endsWith(mainFile, '.js')){
      mainFiles.push(mainFile);
    }
  });

  // run the gulp stream
  return gulp.src(mainFiles)
    .pipe(concat('libs.js'))
    .pipe(gulp.dest('./dist'));
});
{% endhighlight %}

## Explanation

It's a bit long. Let me break down the code and explain it.

First, the `exclude` array is for you to specify the library name that you don't
want to bundle into the main library file.

{% highlight js %}
var exclude = ['lib1', 'lib2'];
{% endhighlight %}

Next, we get the information about the dependencies (list of libraries) that our
project needs

{% highlight js %}
var bowerFile = require('./bower.json');
var bowerPackages = bowerFile.dependencies;
{% endhighlight %}

And next, we define some variables. `packagesOrder` will store the right order
of the libraries (dependencies first). `mainFiles` will store the list of all
main files of the libraries (in the right order) to add to the bundle file

{% highlight js %}
var bowerDir = './bower_components';
var packagesOrder = [];
var mainFiles = [];
{% endhighlight %}

Next, iterate through all bower packages that the project needs and add it to
the `packagesOrder` array using the `addPackage` function except the ones listed
in  `exclude`.

{% highlight js %}
// calculate the order of packages
underscore.each(bowerPackages, function(value, key){
  if(exclude.indexOf(key) === -1){ // add to packagesOrder if it's not in exclude
    addPackage(key);
  }
});
{% endhighlight %}

The `addPackage` function parses the **bower.json** file of the library passed
into it, get all the dependencies and repeat the add step by calling itself.

{% highlight js %}
function addPackage(name){
  // package info and dependencies
  var info = require(bowerDir + '/' + name + '/bower.json');
  var dependencies = info.dependencies;
  
  // add dependencies by repeat the step
  if(!!dependencies){
    underscore.each(dependencies, function(value, key){
      if(exclude.indexOf(key) === -1){ // add only it's not in exclude
        addPackage(key);
      }
    });
  }
  
  // and then add this package into the packagesOrder array if they are not exist yet
  if(packagesOrder.indexOf(name) === -1){
    packagesOrder.push(name);
  }
}
{% endhighlight %}

After that, iterate through the `packagesOrder`. For each library, read its
**bower.json** to get the `main` property. Add that main file to `mainFiles` if
that is a .js file

{% highlight js %}
underscore.each(packagesOrder, function(bowerPackage){
  var info = require(bowerDir + '/' + bowerPackage + '/bower.json');
  var main = info.main;
  var mainFile = main;

  // get only the .js file if mainFile is an array
  if(underscore.isArray(main)){
    underscore.each(main, function(file){
      if(underscoreStr.endsWith(file, '.js')){
        mainFile = file;
      }
    });
  }

  // make the full path
  mainFile = bowerDir + '/' + bowerPackage + '/' + mainFile;

  // only add the main file if it's a js file
  if(underscoreStr.endsWith(mainFile, '.js')){
    mainFiles.push(mainFile);
  }
});
{% endhighlight %}

Finally, concatenate all the files in `mainFiles` into one file

{% highlight js %}
return gulp.src(mainFiles)
  .pipe(concat('libs.js'))
  .pipe(gulp.dest('./dist'));
{% endhighlight %}

The result when you run this task is a **libs.js** file in **dist** folder. All
you need to do is to include that **libs.js** in you website layout.
