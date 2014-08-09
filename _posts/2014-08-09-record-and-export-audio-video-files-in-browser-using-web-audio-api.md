---
layout: post
title: "Record and Export Audio, Video files in browser using Web Audio API with Recorder.js"
description: ""
categories: [javascript]
tags: []
---
{% include JB/setup %}

> The Web Audio API provides a powerful and versatile system for controlling
> audio on the Web, allowing developers to choose audio sources, add effects to
> audio, create audio visualizations, apply spatial effects (such as panning)
> and much more.
> 
> [Web Audio API - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API)

# Record Audio and Export to WAV files with Recorder.js

[Recorder.js](https://github.com/mattdiamond/Recorderjs) is a library for
recording and exporting the output of Web Audio API. It can be installed using
[Bower](http://bower.io/ ) using the package name `recorderjs` (not
`recorder.js`, it's another library). After installation, load the
**bower_components/Recorderjs/recorder.js** file to your html file to use.

**Note**: inside the **Recorderjs**, there is another file named
**recorderWorker.js**. You will need to specify the path to that file later in
Javascript code. You should run this under an http web server, not through
`file:///`.

In the HTML page, I have 2 buttons for start and stop Recording

{% highlight html %}
<button onclick="record()">Record</button>
<button onclick="stop()">Stop</button>
{% endhighlight %}

Next, in the Javascript code, you need to add this to the beginning of your
script file to fix the browser compatibility.

<!-- more -->

{% highlight js %}
var navigator = window.navigator;
navigator.getUserMedia = (
  navigator.getUserMedia ||
    navigator.webkitGetUserMedia ||
    navigator.mozGetUserMedia ||
    navigator.msGetUserMedia
);

var Context = window.AudioContext || window.webkitAudioContext;
var context = new Context();
{% endhighlight %}

The `navigator.getUserMedia()` will prompt the user for permission to use media
devices like camera or microphone and then returns a `LocalMediaStream` in its
callback.

The `AudioContext` is used for creating, processing and decoding audio.
According to
[Mozilla](https://developer.mozilla.org/en-US/docs/Web/API/AudioContext), you
need to create an `AudioContext` before you can do anything because everything
happens inside a context.

Next, define the function for handling start Recording

{% highlight js %}
// we need these variables for later use with the stop function
var mediaStream;
var rec;

function record() {
  // ask for permission and start recording
  navigator.getUserMedia({audio: true}, function(localMediaStream){
    mediaStream = localMediaStream;

    // create a stream source to pass to Recorder.js
    var mediaStreamSource = context.createMediaStreamSource(localMediaStream);

    // create new instance of Recorder.js using the mediaStreamSource
    rec = new Recorder(mediaStreamSource, {
      // pass the path to recorderWorker.js file here
      workerPath: '/bower_components/Recorderjs/recorderWorker.js'
    });

    // start recording
    rec.record();
  }, function(err){
    console.log('Browser not supported');
  });
}
{% endhighlight %}

Finally, we need another function for stopping Recording and export the WAV file

{% highlight js %}
function stop() {
  // stop the media stream
  mediaStream.stop();

  // stop Recorder.js
  rec.stop();

  // export it to WAV
  rec.exportWAV(function(e){
    rec.clear();
    Recorder.forceDownload(e, "filename.wav");
  });
}
{% endhighlight %}

Now, run a simple http server in the directory that you are working and open the
file to see the result. If you have Nodejs and npm installed, you can install a
simple, zero configuration http server using `npm install -g http-server`. The
full source code for this is included at the end of this post.

# Record and Stream video

Recording video using Web Audio API is similar to that of recording audio file,
you just need to add the property `video: true` to the option object that passed
to `getUserMedia()`. This time, there is no need for using Recorder.js since we
don't need to export anything.

In the HTML code, similarly, add two buttons for start and stop Recording. Add
one `<video>` element for playing the video also

{% highlight html %}
<button onclick="recordVideo()">Record Video</button>
<button onclick="stopVideo()">Stop Video</button>

<video src="" width="500" height="500">
{% endhighlight %}

The Javascript code is pretty similar to the previous one.

{% highlight js %}
var videoMediaStream;
var video;

function recordVideo() {
  // ask for permission and start recording
  navigator.getUserMedia({video: true, audio: true}, function(localMediaStream){
    // create a media source from the media stream
    videoMediaStream = localMediaStream;
    var mediaStreamSource = context.createMediaStreamSource(localMediaStream);

    // get the video element from the web page
    video = document.querySelector('video');
    // set the URL of the video to the mediaStramSource
    video.src = URL.createObjectURL(localMediaStream);
    // start playing
    video.play();
  }, function(err){
    console.log('Browser not supported');
  });
}

function stopVideo() {
  video.pause();
  videoMediaStream.stop();
}
{% endhighlight %}

**Note**: for the example of recording, please turn off your speaker or plugin
your headphone, otherwise, the sound recorded to the computer will continues to
go out through the speaker and the recording step is repeated again endlessly.
That will cause some extremely terrible sound that you don't want to hear.

# Demo and Source code

- Live demo is embeded directly inside the below iframe

<iframe width="350" height="350" src="/files/2014-08-09-record-and-export-audio-video-files-in-browser-using-web-audio-api/index.html">
</iframe>

- Full source code is available on Github [web-audio-example](https://github.com/tmtxt/web-audio-example).
