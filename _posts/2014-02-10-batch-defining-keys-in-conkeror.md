---
layout: post
title: "Batch Defining keys in Conkeror"
description: ""
category: conkeror
tags: [conkeror, web-browser]
---
{% include JB/setup %}

# The problem

Usually, in Conkeror, when you want to bind a keystroke to a command or function
in a key map, you would use the `define_key` function (see more at
[Conkeror Key Bindings](http://conkeror.org/KeyBindings)). However, when you
have too many key bindings, it will lead to the problem of repetition in your
config code. For example

{% highlight js %}
define_key(default_global_keymap, "A-z", previous_buffer);
define_key(default_global_keymap, "C-j", previous_buffer);
define_key(default_global_keymap, "A-x", next_buffer);
define_key(default_global_keymap, "C-l", next_buffer);
{% endhighlight %}

This is extremely annoying since I have to rebind too many keys in Conkeror (the
Emacs style keys is inefficient). Luckily, the flexibility of Javascript allows
us dynamically define the number of arguments that passed to the function. By
using that, you can define your own function for binding the keys to avoid
repetition in your init code.

<!-- more -->

# Defining keys function

The idea is to create a function that receive a key map followed by pairs of
keystroke and the command/function that bound to it. The function looks like
this

{% highlight js %}
function my_define_keys(){
  // check if arguments if larger than 1
  if(arguments.length > 1){
	var args = Array.prototype.slice.call(arguments);
	// get the keymap
	var keymap = args[0];
	// remove the first item (keymap)
	args.splice(0, 1);
	// the length
	var length = 0;
	// check the number of elements in arguments
	if(args.length % 2 == 0){
	  length = args.length;
	} else {
	  length = args.length + 1;
	  // add one more null item to the array
	  args[length] = null;
	}
	// loop through the arguments
	for(var i = 0; i < length; i+=2){
	  define_key(keymap, args[i], args[i+1]);
	}
  }
}
{% endhighlight %}

Now, if you want to batch define keys, just pass the keymap as the first
argument, and the rest will be the pairs of keystroke and the command. The
following example demonstrates how to batch define keys for `default_global_keymap`.

{% highlight js %}
my_define_keys(default_global_keymap,
				  "A-z",		previous_buffer,
				  "C-j",		previous_buffer,
				  "A-x",		next_buffer,
				  "C-l",		next_buffer,
				  "O",			"find-url-new-buffer",
				  "C-x C-d",	"find-alternate-url",
				  "A-q",		"quit",
				  "C-tab",		"switch-to-recent-buffer",
				  "C-S-tab",	"switch-to-last-buffer",
				  "C-A-x",		switch_to_last_tab,
				  "0",			switch_to_last_tab,
				  "A-n",		"colors-toggle",
				  "C-R",		"show-tab-temporarily",
				  "w",			"tmtxt-close-and-save-current-buffer",
				  "A-w",		"tmtxt-close-and-save-current-buffer",
				  "A-k",		"tmtxt-close-and-save-current-buffer",
				  "A-W",		"tmtxt-open-closed-buffer",
				  "A-T",		"tmtxt-open-closed-buffer",
				  "A-h",		"stop-loading-all-buffers",
				  "A-r",		"reload-all-buffers"
				 );
{% endhighlight %}

You can pass as many key bindings as you want. The argument that follow right
after the keystroke is the string representing the interactive command name
(which can be activated via **M-x**) or the name of the interactive function
that you want to bind.

This function is very helpful. It eliminates the repetition in my code and makes
my code a lot easier to read.

# Undefining keys function

Similarly, the function for undefining keys receives the key map as the first
argument and followed by a series of keystroke that you wish to unbind from that
key map. This is how the function looks like

{% highlight js %}
function my_undefine_keys(){
  var args = Array.prototype.slice.call(arguments);
  // check if the arguments number is bigger than 1
  if(args.length > 1){
	var keymap = args[0];
	args.splice(0,1);
	// loop through the args
	for(var i = 0; i < args.length; i++){
	  undefine_key(keymap, args[i]);
	}
  }
}
{% endhighlight %}

Usage example

{% highlight js %}
my_undefine_keys(content_buffer_normal_keymap, "F", "C-f", "C-b", "C-p", "C-n", "M-v");
{% endhighlight %}

# Function for defining key aliases

This function behaves much in the same way with the defining keys function.

{% highlight js %}
function my_define_keys_aliases(){
  var args = Array.prototype.slice.call(arguments);
  // check if the number of arguments is even
  var length;
  if(args.length % 2 == 0){
	length = args.length;
  } else {
	length = args.length + 1;
	args[length] = null;
  }
  // loop through the args
  for(var i = 0; i < length; i+=2){
	define_key_alias(args[i], args[i+1]);
  }
}
{% endhighlight %}

In the following example, `C-o` is aliased to `escape`, `A-v` is aliased to
`C-y` and so on.

{% highlight js %}
my_define_keys_aliases("C-o",			"escape",
						  "A-v",			"C-y",
						  "A-c",			"M-w",
						  "C-m",			"return",
						  "M-L",			"C-e",
						  "M-J",			"C-a",
						  "C-J",			"C-A-z",
						  "C-L",			"C-A-x",
						  "C-i",			"tab");
{% endhighlight %}
