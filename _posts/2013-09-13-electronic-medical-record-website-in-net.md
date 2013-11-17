---
layout: post
title: "Electronic Medical Record website in .Net"
description: ""
category: Misc
thumbnail: 
showtn: no
tags: [.net, c#, asp.net]
---
{% include JB/setup %}

> This is one of my assignment at RMIT University Vietnam

I've just submitted my assignment in .Net course at RMIT. The assignment asks me
to implement an Electronic Medical Record website using ASP.Net. I have put much
effort on this website (although I don't like .Net and Windows). The source code
is host on Github and you are free to use it at your own risk.

# Overview of the website

The website is about an Electronic Medical Record (EMR) system that support
management patient treatment in medical facility such as clinic or hospital.
Detail requirements can be found in the pdf file inside the repo on github.

# Functional details

The website allows users to manage and maintain the record of medical services
provided by various hospitals. It focuses mainly on patients and their visit
history as well as the symptoms, prescriptions and lab orders of each visit.

These are some main entities in the application: Patient, Doctor, Hospital,
Visit, Prescription, Lab Order, Drug,
Drug Group, Medical Service, Medical Service Group, ICD, ICD Chapter and User. For
each entity, the website provides 6 basic operations: add new, update, delete,
view all, view one item and search.

<!-- more -->

# Design details

- ASP.NET membership API is applied for authentication and authorization
- The website uses SQL server database and LINQ-2-SQL for connecting to the
  database
- Most operations is done via asynchronous postback
- AJAX Control Toolkit is used for date picker
- Master Page is built with bootstrap 2.3 for consistency layout

# Source code

The source code is hosted on Github at
[Electronic Medical Record](https://github.com/tmtxt/electronic-medical-record).
Simply clone it into your local computer and run.

# Requirements to run

- Microsoft Visual Studio 2012+
- Microsoft .Net Framework 4.5+
- Microsoft SQL Server 2008+

# Database Deployment Instruction

Use SQL Server Studio to open the file "db.sql" inside "ElectronicMedicalRecord"
folder. Execute all the script to generate the database.

# Website Deployment Instruction

- Open file "ElectronicMedicalRecord.sln" in Visual Studio.
- Open "web.config" file and edit the connection information to the new database
location as well as SQL server authentication information correspond to the
information on your computer
- View the Website in browser
- In case you encounter error, switch to IIS Express. In Visual Studio 2012, click
on the menu Website > Use IIS Express

# Sample accounts for testing

- Admin: Username: admin - Password: adminadmin!
- User: Username: user - Password: useruser!

# Demo Images

Here are some images of the website

![Alt Text](/files/2013-09-13-emacs-dired-new-terminal-window-at-current-directory-on-macos/login1.png)

![Alt Text](/files/2013-09-13-emacs-dired-new-terminal-window-at-current-directory-on-macos/login2.png)

![Alt Text](/files/2013-09-13-emacs-dired-new-terminal-window-at-current-directory-on-macos/home.png)

![Alt Text](/files/2013-09-13-emacs-dired-new-terminal-window-at-current-directory-on-macos/list.png)

![Alt Text](/files/2013-09-13-emacs-dired-new-terminal-window-at-current-directory-on-macos/new.png)

![Alt Text](/files/2013-09-13-emacs-dired-new-terminal-window-at-current-directory-on-macos/detail.png)

![Alt Text](/files/2013-09-13-emacs-dired-new-terminal-window-at-current-directory-on-macos/visit1.png)

![Alt Text](/files/2013-09-13-emacs-dired-new-terminal-window-at-current-directory-on-macos/visit2.png)
