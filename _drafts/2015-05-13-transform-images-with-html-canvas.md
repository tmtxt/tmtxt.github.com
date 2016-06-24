---
layout: post
title: "Transform and Export Images with HTML Canvas"
description: ""
categories: []
tags: [javascript]
thumbnail: 
---


# HTML5 Canvas and Image Manipulation

[Canvas](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API) is a great
feature that HTML5 brings to us. With its powerful APIs, we can manipulate
the image in many different ways that you can imagine.

This post is a
collection of my experience when in working with Image Transforming using
Canvas. Most of them exploit the code from Mozilla tutorials like
[Using Images](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Using_images)
or
[Pixel Manipulation with Canvas](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Pixel_manipulation_with_canvas).

The post also includes some tips for working with images like uploading them to
server, export to file or compress bulk of files to download to local computer.

# Load the source image

First step, we need to load the image we want to process. The image can be
retrieved from a file, a URL, a data URL or from an `<img>` component on the web
page.

{% highlight js %}
var image = new Image();
image.onload = function() {
  process(image); // mentioned in the next sections
};
image.src = 'http://example.com/img.png';
{% endhighlight %}

The next sections will focus on the content of the `process` function only. At
the end of the post, there will be instruction on how to export the image.

# Resize Image

To resize and image, simply draw it to a canvas with the desired size and then
let the canvas export it for you

{% highlight js %}
// create a virtual canvas
var canvas = document.createElement('canvas');

// set the width and height to the desired size
// this will resize the image to 1/4 of the original size
canvas.width = image.width / 2;
canvas.height = image.height / 2;

// draw the image
var ctx = canvas.getContext('2d');
ctx.drawImage(img, 0, 0, canvas.width, canvas.height);

// export the new image
console.log(canvas.toDataURL());
{% endhighlight %}

Open the javascript console and click on the link printed in the console to see
you new image.

# Crop Image

Cropping and image is similar to resizing an image, you also need to draw that
on a canvas, but just draw a portion of the image and then let the canvas export
the image for you.

{% highlight js %}
// create a virtual canvas
var canvas = document.createElement('canvas');

// set the width and height to the desired size
canvas.width = cropWidth;
canvas.height = cropHeight;
{% endhighlight %}
