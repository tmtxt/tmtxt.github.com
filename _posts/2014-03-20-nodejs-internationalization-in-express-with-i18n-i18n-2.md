---
layout: post
title: "Nodejs - Internationalization in Express with i18n and i18n-2"
description: ""
categories: [javascript]
tags: []
---


Internationalization (i18n) is one essential part of any application. This can
be achieved easily in Nodejs with the the module
[i18n](https://github.com/mashpie/i18n-node). There is another module called
[i18n-2](https://github.com/jeresig/i18n-node-2), which is based on **i18n** and
designed specifically to work out-of-the-box with Express.js. In this post, I
will focus mostly on **i18n-2**.

First, you need to install **i18n-2** using npm

{% highlight console %}
$ npm install --save i18n-2
{% endhighlight %}

After that, add these config line into your `app.js` file. Remember to add it
after you have loaded the `cookieParser`.

{% highlight js %}
app.use(express.cookieParser('your secret here')); // put the config after this line

i18n.expressBind(app, {
  // setup some locales - other locales default to vi silently
  locales: ['vi', 'en'],
  // set the default locale
  defaultLocale: 'vi',
  // set the cookie name
  cookieName: 'locale'
});

// set up the middleware
app.use(function(req, res, next) {
  req.i18n.setLocaleFromQuery();
  req.i18n.setLocaleFromCookie();
  next();
});
{% endhighlight %}

<!-- more -->

The **i18n** object will now reside within the request object of each request.
The above config also allows the locale to be set from query string or from
cookie. For example, the `mysite.com/?lang=en` will automatically set the locale
to `en`. To use the **i18n** object, simply use the `__` function

{% highlight js %}
function handlerFunc1(req, res){
  res.render('index', { title: req.i18n.__("hello") });
}
{% endhighlight %}

Or if you want to use it in your view, simply use `__` again

{% highlight css+php %}
<h1>
  <%= __("hello") %>
</h1>
{% endhighlight %}

**i18n-2** will then look up the key **hello** in the locale files (by default
located in **locales/en.js** and **locales/vi.js**). If the keys or the files is
not exist yet, it will then create those files and keys automatically for you so
you don't have to worry about the errors. You can then open those files and edit
the values already there or add your new one. The file syntax is just normal
JSON syntax.

**Note**: you cannot use `//` for comments.

{% highlight js %}
{
  "hello": "Hello",
  "title": "title",
}
{% endhighlight %}

To change the language, you can set it directly using `setLocale(locale)`
function. Beside that, you can set the cookie `locale` value for the browser to
remember the current language for the next access.

{% highlight js %}
function handlerFunc(req, res){
  // you can set it directly like this
  req.i18n.setLocale('vi');

  // or set it via the cookie
  res.cookie('locale', 'vi');
  req.i18n.setLocaleFromCookie();

  // redirect back
  res.redirect('back');
};
{% endhighlight %}
