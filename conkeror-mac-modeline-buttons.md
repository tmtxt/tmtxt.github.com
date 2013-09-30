---
layout: page
title: "Conkeror Mac Modeline Buttons - CMMB"
description: "An extension to show buttons in Modeline for Conkeror"
group: project
---
{% include JB/setup %}

# Modeline buttons in Conkeror

**Conkeror Mac Modeline Buttons** - **CMMB** is an extension (based on the built-in
mode-line-buttons.js) for Conkeror to display any buttons that you want in the
Modeline.

![Demo](/files/conkeror-mac-modeline-buttons/demo.png)

Conkeror is a keyboard-oriented browser. I'm sure many of you when you see
this page will argue that it's designed for the keyboard and it's much faster
when we interact with it through keyboard. I totally agree! So what is the
reason why I develop this package? Well, look at these pictures below to see why

Sometimes, we use the computer this way

![Alt Text](/files/conkeror-mac-modeline-buttons/use2.jpeg)\ 
![Alt Text](/files/conkeror-mac-modeline-buttons/use3.jpg)

Or even this crazy way :LOL:

![Alt Text](/files/conkeror-mac-modeline-buttons/use4.jpg)

Now you know why this package appears. That is sometimes I'm just lazy
or I'm using the computer on bed or some other reasons, it prevent me from using
the computer with both hands. In that case, I have to either switch to other browser
or change to other place (the desk), which is impossible because I'm lazy at
that time :LOL: This package also acts as a gateway drug to Conkeror for new people.

Let me explain briefly the name of this project: Conkeror Mac Modeline Buttons.
Conkeror already has built-in basic navigation buttons on modeline, which you
can activate using this in your .conkerorrc

{% highlight js %}
load_paths.unshift("chrome://conkeror-contrib/content/");
require("mode-line-buttons.js");
mode_line_add_buttons(standard_mode_line_buttons, true);
{% endhighlight %}

This works fine on Linux. However, on MacOS, there is no **moz-icon://stock/gtk**
so that all the buttons cannot display correctly on MacOS. Moreover, the
built-in function is limited as you can only add and remove button in your init
file, not at run time. Yeah I want more! I'm a Mac user and I developed this
library for using with my Mac so that I named it **Conkeror Mac Modeline Buttons**.
However, I'm pretty sure that you can run it on Linux because they have the
same file structure. I'm not sure if it works on Windows (I have not tested yet)
but in the code, I use the dynamic code for getting home directory, not
hard code, so if you're lucky, it will work.

# Features

### Firefox's default icons

Button icons are extracted from Mozilla Firefox's default icons on Mac OSX Lion

![Alt Text](/files/conkeror-mac-modeline-buttons/toolbar.png)

### Built-in button groups

3 built-in button groups: navigation, editing and download buttons

- **Navigation**: find-url, find-url-new-buffer, back, forward, reload,
  kill-current-buffer, buffer-previous, buffer-next, minibuffer-abort

![Alt Text](/files/conkeror-mac-modeline-buttons/navigation.png)

- **Editing**: cut, copy, paste

![Alt Text](/files/conkeror-mac-modeline-buttons/editing.png)

- **Download** (mostly available in download buffer):
  download-manager-show-builtin-ui, download-cancel, download-retry,
  download-resume, download-pause

![Alt Text](/files/conkeror-mac-modeline-buttons/download.png)

### Multiple button groups

Multiple button groups can be display at the same time by activating their add
commands. You can call any button group's remove command without affecting the
other ones.

![Alt Text](/files/conkeror-mac-modeline-buttons/multi1.png)

![Alt Text](/files/conkeror-mac-modeline-buttons/multi2.png)

### Easily add new button of any command

You can easily add new button group follow the instruction below. Simply add the
interactive command name and it's image into the button definition, define the
command to activate and you're done. Using this one in combination with the tip
on
[how to define interactive command for often used sites](http://conkeror.org/Tips#Keyboard_Shortcuts_for_Often-Used_Sites)
you even add a button groups of your favorite pages.

# Download and Installation

It's very easy to install since you just have to clone it from github. Make sure
you have git installed and your .conkerorrc is a directory (new style).

First, clone it from github into your .conkerorrc

{% highlight console %}
$ cd ~/.conkerorrc
$ git clone https://github.com/tmtxt/conkeror-mac-modeline-buttons.git
{% endhighlight %}

You're encouraged (but not compelled) to clone it into .conkerorrc. In that
case, put this one into your init file to load it

{% highlight js %}
let (path = get_home_directory()) {
  path.appendRelativePath(".conkerorrc");
  path.appendRelativePath("conkeror-mac-modeline-buttons");
  load_paths.unshift(make_uri(path).spec);
{% endhighlight %}

If you put the package outside .conkerorrc folder, use this one

{% highlight js %}
load_paths.unshift("file://path/to/conkeror-mac-modeline-buttons/folder");
require("conkeror-mac-modeline-buttons.js");
{% endhighlight %}

You also have to set the image path if you place the package outside
.conkerorrc. See the Configurations section for more information

# Usage

Simply type M-x and activate one of these commands to show the buttons
- cmmb-add-navigation-buttons (show navigation buttons)
- cmmb-remove-navigation-buttons (hide navigation buttons)
- cmmb-add-editing-buttons (show editing buttons)
- cmmb-remove-editing-buttons (hide navigation buttons)
- cmmb-add-download-buttons (show download buttons)
- cmmb-remove-download-buttons (hide navigation buttons)

You can also bind the commands to any keystroke that you want

# Configurations

If you want to use your own images, you can replace the images
already exist inside the images folder with the ones that you want. You can also
use your own image folder by
setting variable cmmb_image_path. Also, if you have placed the package
outside .conkerorrc, setting this variable is a must. Otherwise, just ignore
this part and leave everything default.

{% highlight js %}
cmmb_image_path = "/path/to/images/folder";
{% endhighlight %}

# Add your custom buttons list

First, create an empty array for holding the widgets

{% highlight js %}
var my_button_widgets = new Array();
{% endhighlight %}

Second, create an array for button definitions

{% highlight js %}
my_buttons = [
    ["find-url", "open"],
    ["find-url-new-buffer", "new"],
    ["back", "go-back"],
    ["forward", "go-forward"],
    ["reload", "refresh"],
    ["kill-current-buffer", "close"],
    ["buffer-previous", "go-up"],
    ["buffer-next", "go-down"],
  ["minibuffer-abort","cancel"],
];
{% endhighlight %}

Each item in the array is also another array.
The first element in that array indicates the interactive command to be executed
when clicking on the button
The second element in that array is the name of the image file (without
extension) in PNG format. The image size should be 20x20.
By default, the images are located under
**~/.conkerorrc/conkeror-mac-modeline-buttons/images**

Next, define an interactive command like this and pass in the two arrays that
you have created before. This command is for adding the buttons
{% highlight js %}
interactive("cmmb-add-my-buttons",
	"Add basic navigation buttons to the mode line",
	function(I){
	  cmmb_add_buttons(my_buttons, true, my_button_widgets);
	});
{% endhighlight %}

After that, define another interactive command for removing modeline buttons
and pass in the widgets array that you have created before

{% highlight js %}
interactive("cmmb-remove-my-buttons",
	"Remove navigation buttons from mode line", 
	function(I){
	  cmmb_remove_buttons(my_button_widgets);
	});
{% endhighlight %}

Finally, bind that two interactive commands to the keystrokes that you want

If you don't want to use interactive commands, just want the buttons to be added
automatically when you start conkeror, replace the interactive commands with

{% highlight js %}
cmmb_add_buttons(my_buttons, true, my_button_widgets);
{% endhighlight %}

You can add many button lists without interfering other buttons list.
