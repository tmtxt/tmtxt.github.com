---
layout: post
title: "Design Microservices app following 12 Factor App"
description: ""
categories: []
tags: []
thumbnail:
---

So you finally decided that you will start developing your app using Microservices architecture (I
don't recommend this), or it's the time for you to break down your large app into smaller components
using Microservices design? If that's the case, you may want to take a look at this post. This blog
post summarises some of my experiences in designing a highly-scalelable application using
Microservices design while working at Agency Revolution. By applying 12-Factor App methodology, we
gain a lot of benefits in designing our app in a way that easy to develop, test, deploy. It also
helps us debug the application very easily by replicating just some small parts of the system in our
local environment with the current production data to see how the system perform in reality.

This post is not really about how to design Microservices app following exactly 12-Factor App.
Instead, it contains many of my experiences and solutions which based on the idea of 12-Factor App.

# Design the Services in a hierachy model

This is quite an interesting and helpful experience. It's best to illustrate this using a picture

picture comes later

As you can see from the above image, each service (component) in the system is organised in a hierarchy structure and is assigned with a level number. This level values are just the convention. The basic idea is that the request can only go from the components at higher level to the lower level. There are no reverse ways. Another thing to notice is that the components with the same level also cannot request to each other. The **request** here can be in various forms. It can be an http request or it can be a database call to execute a query. For example, in this case, **svc.user** can send request to **svc.
