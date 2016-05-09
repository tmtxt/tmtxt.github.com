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

If there are multiple requests that process at the same time, the log entries in the log file could be something like this

* Starting request 1
* Starting request 2
* End request 2
* End request 1

That's hard for me unite all the logs into one place and search the all the related log information whenever debugging one specific request. There is also no way for me to track the flow of execution steps inside the handler function of that request. Although I can simply do `(timbre/info "Start some database queries")`, the problem than come back to the previous one

* Starting request 1
* Starting request 2
* Start a query for request 1
* Start a query for request 2
* Write file for request 2
* Write file for request 1
* End request 2
* End request 1

Hmmm. Something like this would be much better

* [1] Starting request {id}  
[2] Start query to database  
[3] Found one record  
[4] Processing data  
[5] Finished request {id} in 20ms
* [1] Starting request {id}  
[2] Start query to database  
[3] Exception: database down  
[4] Finish request {id} in 10ms

What I want is one single log entry per request with the trace of its steps so I can easily find out how the code works as well as where it can break, where it doesn't function normally.

# The problem with immutable data

So the initial idea is to add a middleware to wrap around the request handler for processing the log. For example

```
(defn wrap-log-trace [handler]
  (fn [request]
    (let [response (handler request)]
      (write-log [request response])
      response)))
```

The problem is that Clojure ultilizes immutable data by default, so if you want to capture all the log trace steps, at least you need to do something like this in your handler function

```
(defn handler [request]
  (let [log-data []
        log-data (conj log-data "Start query")
        _        (db/query user)
        log-data (conj log-data "Start write file")
        _        (file/write "test.txt")
        response {:status 200
                  :body "ok"
                  :log-data log-data}]
        ; return response
        response))
```

Awwwww, probably not a good solution. I would prefer this approach

```
(defn handler [request]
  (log-trace/add "Start querying database")
  (db/query user)
  (log-trace/add "Start writing file")
  (file/write "test.txt")
  ;return response
  {:status 200
   :body "ok"})
```

And then the `wrap-log-trace` middleware should process all the related log information inside that request and write to log file when the request ends.

# Time for some Mutability

So... it's time to use mutable thread local var in Clojure (defined with `^:dynamic`). Now back to the `