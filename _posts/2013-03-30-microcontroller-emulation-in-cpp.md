---
layout: post
title: "Microcontroller Emulation in C++"
description: ""
category: Misc
thumbnail: 
tags: [c++, microcontroller emulation]
---
{% include JB/setup %}

    This is my assignment in the C++ course at RMIT. I posted it here after
    submitting the assignment to share code with other people on how a simple
    emulation system works. You are free to use all those code but I'm not
    responsible for all problems that might happen. Also, I have no
    responsibility if you are caught to use my code in some later RMIT or any
    other university assignment. I just posted it here to show my work and share
    with other people. So... Use it at your own risk.

# Microcontroller Emulation Program

Emulator programs or emulator modules have been around since the first home
computers were released. At the time they were used to be able to run programs
that were designed for other platforms. Nowadays, emulators are used for many
purposes: to emulate old computer systems and arcade machines (e.g. the MAME and
MESS projects); to help with hardware backward compatibility (PS and Xbox360
support old software through emulation); to help with software backward
compatibility (Windows 7 supports old software though Windows XP on x86
emulation); and to run multiple virtual servers on large mainframe systems to
reduce operating costs (virtualisation.)

A microcontroller is a chip that is used to perform simple functions, such as
input, processing and output. Microcontrollers are found in many household
appliances ranging from alarm clocks to videocassette recorders, from car engine
management units to washing machines, from microwave ovens to remote-control air
conditioning units.

The microcontroller is programmable – a programmer loads a program into its
memory, and the microcontroller executes that program. Input and output is
handled by modifying addresses within memory. Both the program and data for the
microcontroller are stored within the same block of memory. This is the common
Von Neumann architecture that you find in all microchip-controlled devices.

When designing a program, a programmer for a microcontroller had to check and
implement the design by hand. Often, this was done with pen and paper, and later
with a piece of software known as a monitor. The monitor allowed the programmer
to modify sections of memory directly, to display everything in memory, or to
execute the contents of memory as if the contents were a program.

# Requirements

* unix-based system: to run makefile
* git: to clone the repo
* g++ compiler

# Download and Install

Clone the repo on github
[Microcontroller Emulation](https://github.com/tommytxtruong/microcontroller-emulation)

{% highlight sh %}
$ git clone https://github.com/tommytxtruong/microcontroller-emulation.git
{% endhighlight %}

To compile, simply run make

{% highlight sh %}
$ cd microcontroller-emulation
$ make
{% endhighlight %}

To execute program, run the output file **main**

# Additional Informations

Requirement files (detail on how the system works) can be found in
**Requirements** directory. **Report** folder
contains those descriptions for implementation, class diagram, etc. To test
program, you can use the test files provided in **Test** files directory.

# Link

**Project Repo on Github**: <https://github.com/tommytxtruong/microcontroller-emulation>
