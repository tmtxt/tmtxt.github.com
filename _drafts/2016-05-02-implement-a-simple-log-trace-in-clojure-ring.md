---
layout: post
title: "Implement a simple log trace in Clojure Ring"
description: ""
categories: [misc]
tags: []
thumbnail:
---
{% include JB/setup %}

# Why normal logging is not enough for me

There are many logging libraries for Clojure and Ring out there which support basic logging per each request that Ring server handles. However, all of them produce multiple log entries per request, one when the request starts and one when the request ends. Also, they cannot log the steps happen inside the handler function's execution. For example, with [Ring-logger](https://github.com/nberger/ring-logger), the default setup logs:

* an :info-level message when a request begins;
* an :info level message when a response is generated without any server errors (i.e. its HTTP status is < 500);
* an :error level message when a response's HTTP status is >= 500;
* an :error level message with a stack trace when an exception is thrown during response generation

If there are multiple requests that process at the same time, the log file could be something like this

* Starting request 1
* Starting request 2
* End request 2
* End request 1

That's hard for me unite the logs into one place and search the all the related log information whenever debugging one specific request. There is also no way for me to track the flow of execution steps inside the handler function of that request. Although I can simply do `(timbre/info "Start some database queries")`, the problem than come back to the previous one

* Starting request 1
* Starting request 2
* End request 2
* End request 1