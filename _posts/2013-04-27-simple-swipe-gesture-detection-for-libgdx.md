---
layout: post
showtn: yes
title: "Simple Swipe Gesture Detection for LibGdx"
description: ""
category: Misc
thumbnail: /files/2013-04-11-simple-gesture-detection-for-libgdx/thumbnail.png
tags: [libgdx, gesture]
---
{% include JB/setup %}

LibGdx has the built in support for Gesture detection. However, the Swipe
Gesture is not well-supported and a bit simple. I have found a solution from
this
[article](http://code.google.com/p/libgdx-users/wiki/SimpleDirectionGestureDetector).
The code on that link contains errors. Here is the fixed version of it.

![Gesture](/files/2013-04-11-simple-gesture-detection-for-libgdx/thumbnail.png)

<!-- more -->

Create a class name SimpleDirectionGestureDetector

{% highlight java %}
public class SimpleDirectionGestureDetector extends GestureDetector {
	public interface DirectionListener {
		void onLeft();

		void onRight();

		void onUp();

		void onDown();
	}

	public SimpleDirectionGestureDetector(DirectionListener directionListener) {
		super(new DirectionGestureListener(directionListener));
	}
	
	private static class DirectionGestureListener extends GestureAdapter{
		DirectionListener directionListener;
		
		public DirectionGestureListener(DirectionListener directionListener){
			this.directionListener = directionListener;
		}
		
		@Override
        public boolean fling(float velocityX, float velocityY, int button) {
			if(Math.abs(velocityX)>Math.abs(velocityY)){
				if(velocityX>0){
						directionListener.onRight();
				}else{
						directionListener.onLeft();
				}
			}else{
				if(velocityY>0){
						directionListener.onDown();
				}else{                                  
						directionListener.onUp();
				}
			}
			return super.fling(velocityX, velocityY, button);
        }

	}

}
{% endhighlight %}

On the create() function of the LibGdx application, put this to activate gesture
handling for your game

{% highlight java %}
Gdx.input.setInputProcessor(new SimpleDirectionGestureDetector(new SimpleDirectionGestureDetector.DirectionListener() {
		
	@Override
	public void onUp() {
		// TODO Auto-generated method stub
	}

	@Override
	public void onRight() {
		// TODO Auto-generated method stub

	}

	@Override
	public void onLeft() {
		// TODO Auto-generated method stub

	}

	@Override
	public void onDown() {
		// TODO Auto-generated method stub

	}
}));
{% endhighlight %}
