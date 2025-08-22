---
layout: post
title: "Storing Client specific data"
description: ""
categories: [misc]
tags: []
thumbnail:
---

If you are developing a multi-tenant or multi-region application, you've probably run into the
situation where you need to store specific data for each client. For example, you might be building
a system for your insurance agency that requires the Policy to behave slightly differently depending
on the user's state, while most of the main behavior of the Policy object remains the same. There
are several ways to do this. In this post, I'm going to examine some strategies for achieving this
and compare the advantages and disadvantages of each solution.

Let's stick with the above Insurance Agency example for the rest of this post. In this scenario, I
need to store these extra information depending on user geographic location

Here are some examples of extra information you might need to store for each state:

- **For WA state:**
	- Earthquake coverage required
	- Minimum deductible: $1,000
	- State-specific tax rate
- **For NSW state:**
	- Flood coverage required
	- Minimum deductible: $500
	- Fire safety compliance certificate
- **For SA state:**
	- Bushfire risk assessment
	- Minimum deductible: $750
	- Local council approval number

# Store as formatted text

# Store as JSON

# Adding extra column to the table

# Vertical partitioning

# Key-Value pair table

# 