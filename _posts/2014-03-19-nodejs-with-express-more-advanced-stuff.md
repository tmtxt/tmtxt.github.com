---
layout: post
title: "Nodejs with Express - More advanced stuff"
description: ""
categories: [javascript]
tags: []
---
{% include JB/setup %}

> **Full series**:
> [Building website with Nodejs - A post from my experience]({%post_url 2014-03-19-building-website-with-nodejs-a-guide-from-my-experience%})  
> **Previous post**: [Nodejs - Express with ejs/stylus basics]({%post_url 2014-03-19-nodejs-express-with-ejsstylus-basic-explaination%})

# Middleware

Simply, middleware are just functions for handling requests from client. Each
request can have be associated with a stack of middleware. That means when you
have several of handler functions associated with one request, they will be
executed sequentially. Back to the example from my previous post

{% highlight js %}
app.get('/', middleware1);

function middleware1(req, res){
  res.render('index', { title: 'Express', check: true });
};
{% endhighlight %}

In this example, there is only one middleware (handler) function for the GET
request to `/`. Now we will add one more middleware, or maybe more if you want

{% highlight js %}
app.get('/', middleware1, middleware2);

function middleware1(req, res, next){
  // do something here
  // ...

  // activate the next middleware, otherwise, the next middleware will never be
  // called and the client will wait until timeout
  next();
};

function middleware2(req, res){
  res.render('index', { title: 'Express', check: true });
};
{% endhighlight %}

<!-- more -->

You can also signal an error by passing it to the first argument of `next()`
function

{% highlight js %}
function middleware1(req, res){
  // do something here
  // ...
  
  next('error message');
};
{% endhighlight %}

Middleware can be used for several purposes such as authentication and
authorization. You can follow the link to the full series at the beginning of
this post and find the post that I wrote about authentication in Express.
Middleware can also be used in setting common variables for displaying in layout
(refer to the next section).

# Layout in Express EJS

For Express 3.x, if you want to use layout, you need to install
[ejs-locals](https://www.npmjs.org/package/ejs-locals).

{% highlight console %}
$ npm install --save ejs-locals
{% endhighlight %}

In the `app.js` file, add these 2 lines before `app.set('view engine', 'ejs');`

{% highlight js %}
// add these 2 lines
var engine = require('ejs-locals');
app.engine('ejs', engine);

// before this line
app.set('view engine', 'ejs');
{% endhighlight %}

Open the **views** folder, create a file named `layout.ejs`

{% highlight css+php %}
<!DOCTYPE html>
<html>
  <head>
    <title>My title</title>
    <link rel='stylesheet' href='/stylesheets/style.css' />
  </head>
  <body>
    <%- body %>
  </body>
</html>
{% endhighlight %}

There is nothing special here except the `<%- body %>` tag. It's the place
holder and will be replaced by the content of the page that uses this layout.
Next, create another page to consume this layout, for example, **index.ejs**

{% highlight css+php %}
<% layout('layout') -%>

<p>The content of the page appears here</p>
{% endhighlight %}

There is one problem with the layout. That is when you want to render a variable
in the layout using `<%= varName %>`. Continue with the above example, if there
is a variable named **varName** inside `layout.ejs` and you want it to display
based on the page that you are viewing, you can not pass the variable through
the the render function normally like this

{% highlight js %}
res.render('index', { title: 'Express', varName: 'value' });
{% endhighlight %}

You will get the error that varName is not defined because the variable is only
available inside the `index.ejs` view, not the layout. To display it in the
layout, you need to assign it to `res.locals`

{% highlight js %}
function handler(req, res, next){
  res.locals.varName = 'value';
  res.render('index', { title: 'Express'});
};
{% endhighlight %}

And then in `layout.ejs` just echo the variable normally

{% highlight css+php %}
<%= varName %>
{% endhighlight %}

For convenience, you can create a middleware to set common values (current
logged in user for instance) and pass it to the routing function.
