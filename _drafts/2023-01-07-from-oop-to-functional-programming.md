---
layout: post
title: "From OOP to Functional programming"
description: ""
categories: [misc]
tags: []
thumbnail:
---

Object-oriented programming (OOP) and functional programming (FP) are two popular programming
paradigms. In recent years, there has been a trend towards FP due to its benefits such as increased
reliability and easier testing. In this article, I'll show some helpful patterns for people coming
from the OOP world.

# Private properties

In object-oriented programming (OOP), a private property is a member variable that is only
accessible within the class in which it is defined. Private properties are often used to store
internal data for an object that should not be directly modified by external code. They are
typically accessed using getter and setter methods, which provide a way for the object to control
how its private data is accessed and modified. Private properties do not exist in the same way in
functional programming as they do in object-oriented programming (OOP).

The solution to this is simply to use a closure to wrap
