---
layout: post
title: "Working with Zoom behavior in D3.js and some notes"
description: ""
category: javascript
tags: [d3js]
---


# Zoom behavior in d3js

Zooming functionality is a very useful feature for working with large and
complicated svg diagram. Often for those types of graph, zoom ability allows
users to have a detail view of one specific part on that graph. Fortunately,
creating [zoom behavior](https://github.com/mbostock/d3/wiki/Zoom-Behavior)
in [d3js](http://d3js.org/) is an uncomplicated task since the library already
takes care of it all for you. Everything you need to do is to apply the `zoom`
function function on the svg element that you want. For example

{% highlight js %}
// create the zoom listener
var zoomListener = d3.behavior.zoom()
  .scaleExtent([0.1, 3])
  .on("zoom", zoomHandler);

// function for handling zoom event
function zoomHandler() {
  vis.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
}

// create the svg
rootSvg = d3.select("#tree-body").append("svg:svg");
/*
  creating your svg image here
*/

// apply the zoom behavior to the svg image
zoomListener(rootSvg);
{% endhighlight %}

<!-- more -->

Let me explain a bit about the above code. You can also refer to the
[d3js document](https://github.com/mbostock/d3/wiki/Zoom-Behavior) while I'm
explaining those functions. First, create a `zoomListener` using
`d3.behavior.zoom()`. This is the main object for controlling zooming and
panning later. D3js automatically creates event listeners for zooming using
mouse or zooming using 2 fingers touch event (on mobile or tablet).
`scaleExtent` specifies the two min and max scaling ratio. Finally, `on` define
the zoom event that calls to `zoomHandler` function.

Now, just open the page and try to scroll the mouse or using two fingers zoom on
touch devices to see the result. You can try the live example from d3js here
[SVG Geometric Zooming](http://bl.ocks.org/mbostock/3680999).

# Remove zoom listener

Unlike registering event listener, removing it is not a straightforward task.
Although the d3js document states that passing a null handler function to the
event listener will remove it, it will not restore the default scrolling,
swiping, panning,... ability. To remove it completely and restore the default
behaviors, you need to unregister all event handlers that d3js has installed on
the element

{% highlight js %}
svg.on("mousedown.zoom", null);
svg.on("mousemove.zoom", null);
svg.on("dblclick.zoom", null);
svg.on("touchstart.zoom", null);
svg.on("wheel.zoom", null);
svg.on("mousewheel.zoom", null);
svg.on("MozMousePixelScroll.zoom", null);
{% endhighlight %}

Thanks [Lars Kotthoff](http://stackoverflow.com/users/1172002/lars-kotthoff) for
providing me the solution
[Unregister zoom listener and restore scroll ability in D3.js](http://stackoverflow.com/questions/22302919/unregister-zoom-listener-and-restore-scroll-ability-in-d3-js/22303160?noredirect=1#22303160).

# Trigger zoom manually

To trigger zoom manually, you need to understand **translate** and **scale**
meaning in d3js. Actually, this is just similar to **translate** and **scale**
function in svg **transform** attribute.

`zoom.translate([translate])` function is used to specify translation vector to
apply. A picture is worth a thousand words so I will use the picture below to explain it.

![translate](/files/2014-03-13-working-with-zoom-behavior-in-d3js-and-some-notes/translate.png)

For example, you have a rectangle with the top left is at (100,200), and you
want to move it to to position (500,500). The arrow specifies the translate
vector and its value should be (500 - 100, 500 - 200) or (400, 300).

Also, you may want to zoom it in or out after translating it to the new
position. If so, use the `scale` function to set the scale ratio.

After you have set those value, you need to call `event` function of the
**zoomListener** with the element as the input to activate the event. You can also
pass a [transition](https://github.com/mbostock/d3/wiki/Transitions) to that
function set your own transition properties. The final code will look like this

{% highlight js %}
// set the translate vector and scale, zoom 2 times bigger
zoomListener.translate([400,300]).scale(2);

// activate the zoom event
// pass in the transition with duration 500ms
zoomListener.event(rootSvg.transition().duration(500));
{% endhighlight %}

# Be careful when using the zoomend event

According to d3js document, For mousewheel events, which happen discretely with
no explicit start and end reported by the browser, events that occur within 50
milliseconds of each other are grouped into a single zoom gesture. That means
when you zoom with mouse, all the 3 zoom events (zoomstart, zoom, zoomend) will
happens sequentially every 50ms. You can verify this by printing to the console
every time those events occur. As a result, it's very hard for if you want to
determine whether the zoom is really ended with d3's built-in zoom events.

A work around is to use a javascript library for communicating sequential
processes (like Clojurescript core.async) named
[js-csp](https://github.com/ubolonton/js-csp). You can just clone the repo and
include it directly in your browser, however, using
[browserify](http://browserify.org/) is recommended.
The library uses some the new feature (generator) that is introduced in ES6 and
only supported in Firefox >= 27 or Chrome >= 28 with special flags.
To avoid this, you will need to
compile your code using [Regenerator](http://facebook.github.io/regenerator/) to
transform it to ES5 standard. Otherwise, you will need to update Firefox to
newer version or turn on **Enable Experimental JavaScript** in `chrome://flags`.

Only all those requirements are satisfied, you can continue to the next step.
You can read more about the library and its API here
[js-csp Basic concepts and API](https://github.com/ubolonton/js-csp/blob/master/doc/basic.md).
The code is taken and modified from this
[example](https://github.com/ubolonton/js-csp/blob/examples/examples/web/mouse-events.html).
The idea is that every time the **zoomend** event take places, a timer will be
invoked. If the next event happens within that timer's timeout, the previous one
will be canceled. When the final timer ends, the task you specify will be
executed.

{% highlight js %}
// load the library
var csp = require('js-csp');

// create channel with a fixed buffer size of 1
var ch = csp.chan(csp.buffers.dropping(1));

// set the handler for zoomend
var zoomListener = d3.behavior.zoom()
  .scaleExtent([0.1, 3])
  .on("zoom", zoomHandler)
  .on("zoomend", zoomEndHandler);

// the handler for zoomend
function zoomEndHandler(){
  // put the task in to the async channel
  csp.putAsync(ch, "value", startFunc);
}

// this function will be activate for the first time the zoomend occurs
function startFunc() {
  // do something here
}

// start the channel
csp.go(function*() {
  for(;;){
    yield csp.take(ch);
    console.log("START");

    for(;;){
      // set timeout larger than 50
      var result = yield csp.alts([ch, csp.timeout(500)]);
      var value = result.value;
      if(value === csp.CLOSED){
        console.log("STOP");

        // if it reaches here, the zoom is actually ended
        // do something here
        
        break;
      }
      
    }
  }
});
{% endhighlight %}
