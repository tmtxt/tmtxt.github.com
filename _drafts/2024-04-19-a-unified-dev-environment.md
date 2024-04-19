---
layout: post
title: "A Unified Dev environment"
description: ""
categories: [misc]
tags: []
---

I have been dreaming about a unified dev environment for everybody in the team, where there's no
more "It works on my machine" issue. The dev environment should be portable, automatically
configured and can be destroyed/recreated as quick as possible. It should also provide all the
necessary tools for the developer as well as enforce everyone in the team to follow the correct
convention.

# My first attempt in university

Here is my first experiment when I was still a student, a classic solution.

![First attempt](/files/2024-04-19-a-unified-dev-environment/first-attempt.png)

In this method, you would run everything normally in your computer. You use a Bash/Ansible script
to run install all the necessary tools as well as set up the development environment. Of course,
this one was the worst (but the best with my knowledge at that time 😆)

- This setup won't be reused for multiple engineers. Each engineer has a very different setup on that machine. The script can mess up the other people's computer easily.
- Even if it's just for me, that also won't work. I install and set up new applications all the time. After a while, when I touch that project again, all the scripts were broken and I don't even remember how to do.

# How I did it in 2014

I began learning how to do it the professional way in my first job after graduated.
The environment is configured using a Virtualbox VM (provisioned by Vagrant), 
which reflects the real setup on the production server. The server/VM runs everything, from the
database server to the main application.

![2014](/files/2024-04-19-a-unified-dev-environment/2014.png)

This time, of course, it was a much better setup
- The VM could be created or destroyed easily.
- The VM reflects the real production server, which is a Linux server while my machine is a Macbook
- The programs installed inside the VM doesn't affect the engineer computer. I don't have to worry about it will mess up all my daily workflow.
