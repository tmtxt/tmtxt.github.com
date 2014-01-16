---
layout: post
showtn: yes
title: "Simple Android TODO App"
description: ""
category: Misc
thumbnail: 
tags: [android]
---
{% include JB/setup %}

> This is one of my assignment in Android course at RMIT. I just want to share my
> work even though this is just a very simple application because this is my first
> Android application. The source code is opened for everyone who is interested
> in. You have the right to freely use my code. However, I'm not responsible for
> any problem about plagiarism that you can encounter if you use it for your
> assignment.

# Overview

This is a simple Task Management Application (To-do list manager). It helps you
in managing and organize your daily works include what you need to do, when is
the due date for the task, who will be involved in the activity, etc.

<!-- more -->

# Main Entities

At the core of the functional requirements of this application are the following
two entities:

**Task**: A task is a to-do entry that must, at a minimum, maintain the
following information:

*Title*: The title of the task (e.g. “Meet with Lecturer”)  
*Due date*: the due date of a task (e.g. March 5, 2012)  
*Note*: Additional notes describing the task to be performed, which might
include the venue at which the task is to be performed, e.g. “meeting
room 14.10.06”  
*Priority Level*: One of three possible values, High, Medium or Low. This
will determine how the Task list is sorted (discussed in more detail below).  
*Collaborators*: List of email addresses identifying individuals who will be
collaborating on this task, as picked from the user’s contacts list.  
*Group*: The category or group that this task belongs to (discussed in more
detail below).  
*Completion Status*: A Boolean value indicating whether or not a given Task has been
completed.  
*Id*: A randomly generated combination of numbers and letters, which
uniquely identifies a title (not visible to the user).  

**Task Group**: Tasks can be organized and grouped together. In addition to its
constituent tasks, each task group has the following information:

*Title*: The name of the Task Group (e.g. Shopping List, To-Read List)  
*Id*: A randomly generated combination of numbers and letters, which
uniquely identify a group.
	
# Database

Local Database: Tasks and Task Groups are to be persisted on a local SQLite
database. Whilst the database can be simple, it must store all information about
Tasks and Task Groups discussed above.

# Functionalities

- **Add or Remove Tasks**: The application allows the creation of an unbounded
set of Tasks each belonging to one of four predefined Task Groups.
  
- **Edit Task Details**: Users are able to edit details about a Task including
the ability to check/uncheck the completion status of a task, as well as add
or remove collaborators.
  
- **View Task Group**: The program can display a list of Tasks in a given Task
Group and allow sorting by either due date, priority or the order the tasks were
added.

# Requirements

Android phones/tablets/computers with Android OS 4.0 or above

# Download and Installation

To install the software, follow this link
[https://github.com/tmtxt/iDo/blob/master/build/iDo.apk](https://github.com/tmtxt/iDo/blob/master/build/iDo.apk) and select
*View Raw* to download. After finishing the download, copy that file into phone
memory and install it. You're ready to use the application.

# Source code

The source code this application is hosted on Github at this
[link](https://github.com/tmtxt/iDo). It is an Eclipse project. To
explore it, clone it into your local computer and import into Eclipse.

# Screenshots

This is some of the application screenshots from my Samsung Galaxy Note 1

![Screenshot](/files/2013-04-04-simple-android-todo-app/ss9.png)  
Application in Launcher

![Screenshot](/files/2013-04-04-simple-android-todo-app/ss8.png)  
Main screen

![Screenshot](/files/2013-04-04-simple-android-todo-app/ss7.png)  
View all groups

![Screenshot](/files/2013-04-04-simple-android-todo-app/ss6.png)  
Add group

![Screenshot](/files/2013-04-04-simple-android-todo-app/ss5.png)  
Add task

![Screenshot](/files/2013-04-04-simple-android-todo-app/ss4.png)  
Add task

![Screenshot](/files/2013-04-04-simple-android-todo-app/ss3.png)  
View all tasks

![Screenshot](/files/2013-04-04-simple-android-todo-app/ss2.png)  
View task detail

![Screenshot](/files/2013-04-04-simple-android-todo-app/ss1.png)  
Delete task
