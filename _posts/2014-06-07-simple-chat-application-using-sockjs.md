---
layout: post
title: "Simple chat application using SockJS"
description: ""
categories: [javascript]
tags: []
---


# SockJS

[SockJS](https://github.com/sockjs) is a Javascript library that provides a
WebSocket-like object, allows you to create real-time, low-latency, full duplex
and cross-domain communication. SockJS tries to use WebSocket in the background
if the browser supports so the syntax is very similar to WebSocket object.

This post demonstrate a simple chat example using SockJS with NodeJS server. For
other kinds of server, you can find it on the SockJS Github page
[https://github.com/sockjs](https://github.com/sockjs).

SockJS include 2 parts, client-side SockJS and server-side SockJS. You need both
for your application to run properly.

# Server-side SockJS

The SockJS server will listen for all connection on port 9999 (you can change to
whatever you want). Every time a client send a chat message to the server, it
will broadcast to all other clients.

Create a new folder `sockjs-server` for your SockJS server. Create a file name
`server.js`. First, you need to load all the required dependencies. Add this to
your `server.js` file.

{% highlight js %}
// Required packages
var http = require('http');
var sockjs = require('sockjs');
{% endhighlight %}

<!-- more -->

You also need an object to hold a list of all connected clients

{% highlight js %}
// Clients list
var clients = {};
{% endhighlight %}

This function is used to broadcast message to all clients

{% highlight js %}
// Broadcast to all clients
function broadcast(message){
  // iterate through each client in clients object
  for (var client in clients){
    // send the message to that client
    clients[client].write(JSON.stringify(message));
  }
}
{% endhighlight %}

Create a sockjs server like this

{% highlight js %}
// create sockjs server
var echo = sockjs.createServer();

// on new connection event
echo.on('connection', function(conn) {

  // add this client to clients object
  clients[conn.id] = conn;

  // on receive new data from client event
  conn.on('data', function(message) {
    console.log(message);
    broadcast(JSON.parse(message));
  });

  // on connection close event
  conn.on('close', function() {
    delete clients[conn.id];
  });
  
});
{% endhighlight %}

Create an http server and integrate sockjs inside that, listening on port 9999.
The client will need to connect to `http://localhost:9999/echo` (as specifiec in
the `prefix` configuration).

{% highlight js %}
// Create an http server
var server = http.createServer();

// Integrate SockJS and listen on /echo
echo.installHandlers(server, {prefix:'/echo'});

// Start server
server.listen(9999, '0.0.0.0');
{% endhighlight %}

You can download the file `server.js` from this
[link](/files/2014-06-05-simple-chat-application-using-sockjs/server/server.js).

Open Terminal, change to `sockjs-server` folder and issue these commands to
run the server

{% highlight console %}
$ cd sockjs-server
$ npm install sockjs
$ node server.js
SockJS v0.3.9 bound to "/echo"
{% endhighlight %}

# Client-side SockJS

Create another folder `sockjs-client` for client-side chat application. Make a
new file index.html with the content like this

{% highlight html %}
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <title>Sock chat</title>
  </head>
  <body>
    <textarea id="chat-content" style="width:500px;height:300px"></textarea><br/>
    <input type="text" id="username" placeholder="Choose username"/>
    <input type="text" id="message" placeholder="Enter chat message"/>
    <input type="button" value="Send" onclick="sendMessage()"/>

    <script src="http://code.jquery.com/jquery-1.11.0.min.js"></script>
    <script src="http://cdn.sockjs.org/sockjs-0.3.min.js"></script>
    <script src="index.js" ></script>
  </body>
</html>
{% endhighlight %}

Create another `index.js` file for the JS code. First, you need to create a
connection to `http://localhost:9999/echo`.

{% highlight js %}
// Create a connection to http://localhost:9999/echo
var sock = new SockJS('http://localhost:9999/echo');

// Open the connection
sock.onopen = function() {
  console.log('open');
};

// On connection close
sock.onclose = function() {
  console.log('close');
};
{% endhighlight %}

When receive message from the server, show it in the textarea

{% highlight js %}
// On receive message from server
sock.onmessage = function(e) {
  // Get the content
  var content = JSON.parse(e.data);

  // Append the text to text area (using jQuery)
  $('#chat-content').val(function(i, text){
    return text + 'User ' + content.username + ': ' + content.message + '\n';
  });
  
};
{% endhighlight %}

This function is for handling **Send** button click event

{% highlight js %}
// Function for sending the message to server
function sendMessage(){
  // Get the content from the textbox
  var message = $('#message').val();
  var username = $('#username').val();

  // The object to send
  var send = {
    message: message,
    username: username
  };

  // Send it now
  sock.send(JSON.stringify(send));
}

{% endhighlight %}

These are the link for
[index.html](/files/2014-06-05-simple-chat-application-using-sockjs/client/index.html)
and
[index.js](/files/2014-06-05-simple-chat-application-using-sockjs/client/index.js).

To run the chat application, simply open `index.html` file in 2 browser windows,
enter the username, chat message and then click **Send** button.

# Demo Image

![SockJS chat](/files/2014-06-05-simple-chat-application-using-sockjs/chat.gif)
