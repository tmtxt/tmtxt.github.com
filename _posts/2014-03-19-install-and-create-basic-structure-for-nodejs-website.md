---
layout: post
title: "Install and Create basic structure for Nodejs website"
description: ""
categories: [javascript]
tags: []
---
{% include JB/setup %}

> **Full series**:
> [Building website with Nodejs - A post from my experience]({%post_url 2014-03-19-building-website-with-nodejs-a-guide-from-my-experience%})

# Install Nodejs, npm using nvm

There are several ways of installing Nodejs. You can install it using your OS
packages manager or compiling it from source. However, the recommended way is to
install it using [nvm](https://github.com/creationix/nvm). **nvm** can help you
install, maintain and run multiple versions of Nodejs (without sudo need).

To install nvm, simply do

{% highlight console %}
$ git clone https://github.com/creationix/nvm.git ~/.nvm
{% endhighlight %}

Add this to your shell's rc file

{% highlight console %}
$ source ~/.nvm/nvm.sh
{% endhighlight %}

Next, install one version of nodejs that you want using nvm and set it as the default

{% highlight console %}
$ nvm install 0.10
$ nvm alias default 0.10
{% endhighlight %}

Installing nodejs with nvm will automatically install
[npm](https://www.npmjs.org/) (packages manager for Node). For more information
about nvm, visit it's homepage at
[https://github.com/creationix/nvm](https://github.com/creationix/nvm).

To test whether nodejs works correctly, you can use the example from nodejs
page. Create a file named example.js

<!-- more -->

{% highlight js %}
var http = require('http');
http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}).listen(1337, '127.0.0.1');
console.log('Server running at http://127.0.0.1:1337/');
{% endhighlight %}

and then run it using nodejs

{% highlight console %}
$ node example.js
Server running at http://127.0.0.1:1337/
{% endhighlight %}

# Auto reload app with nodemon

Usually, every time you change your code, you have to manually restart node
server in order for the changes to take effect. [Nodemon](https://github.com/remy/nodemon)
is a utility that will
monitor for any changes in your source and automatically restart your server.
Just use `nodemon` instead of `node` to run your code. To install it

{% highlight console %}
$ npm install -g nodemon
$ nodemon example.js
{% endhighlight %}

# Install Express and generate the website structure

[Express](http://expressjs.com/) is a minimal and flexible node.js web
application framework, providing a robust set of features for building single
and multi-page, and hybrid web applications. You can install Express with npm

{% highlight console %}
$ npm install -g express-generator
{% endhighlight %}

Next, use the newly installed express to generate basic site structure. Replace
`myapp` in the command below with your desired app name

{% highlight console %}
$ express --sessions --css stylus --ejs myapp
$ cd myapp && npm install
{% endhighlight %}

The first command will generate a skeleton for your web app with session,
[stylus css](http://learnboost.github.io/stylus/) and
[ejs render engine](http://embeddedjs.com/) support. The second one will install
all the dependencies that you app needs. To run the website, execute `app.js`
using nodejs or nodemon

{% highlight console %}
$ nodemon app.js
19 Mar 13:56:01 - [nodemon] v1.0.15
19 Mar 13:56:01 - [nodemon] to restart at any time, enter `rs`
19 Mar 13:56:01 - [nodemon] watching: *.*
19 Mar 13:56:01 - [nodemon] starting `node app.js`
Express server listening on port 3000
{% endhighlight %}

Open your web browser and go to [http://localhost:3000](http://localhost:3000).
You should see something like this

![Alt Text](/files/2014-03-19-install-and-create-basic-structure-for-nodejs-website/express.png)

> **Next**: [Nodejs - Express with ejs/stylus basics]({%post_url 2014-03-19-nodejs-express-with-ejsstylus-basic-explaination%})
