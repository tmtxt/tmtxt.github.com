---
layout: post
title: "A Unified Development Environment Journey - Part 1"
description: "My dream about a unified devlopment environment for everybody in the team for the last 10 years..."
categories: [misc]
tags: []
---

I have been dreaming about a unified dev environment for everybody in the team, where there's no
more **It works on my machine** issue. The dev environment should be portable, automatically
configured and can be destroyed/recreated as quickly as possible. It should also provide all the
necessary tools for the developer as well as enforce everyone in the team to follow the correct
convention consistently.

# My first attempt in university

Here is my first effort when I was a student, a classic solution.

[//]: # (The image contains embedded scene for Excalidraw)
![First attempt](/files/2024-04-19-a-unified-dev-environment/first-attempt.png)

In this method, you would run everything normally in your computer and use a Bash/Ansible script
to install all the necessary tools as well as set up the development environment. Of course,
this is the worst (but was the best with my knowledge at that time 😆).

- This setup won't be reused for multiple engineers. Each engineer has a very different setup on
  that machine. The script can mess up the other people's computer easily.
- Even if it's just for me, that also won't work. I install and set up new applications all the
  time. After a while, when I touch that project again, all the scripts were broken and I don't even
  remember how to run my app again.

# A better way

I began learning how to do it the professional way in my first job after graduated.
The environment is configured using a Virtualbox VM (provisioned by Vagrant), 
which reflects the real setup on the production server. The server/VM runs everything, from the
database server to the main application.

[//]: # (The image contains embedded scene for Excalidraw)
![2014](/files/2024-04-19-a-unified-dev-environment/before-codespaces.png)

<!-- more -->

Later on, with the increase in Docker adoption, the setup had changed to this

[//]: # (The image contains embedded scene for Excalidraw)
![Docker](/files/2024-04-19-a-unified-dev-environment/docker-workflow.png)

This time, of course, it was a much better setup
- The VM/Containers could be created or destroyed easily.
- The VM/Containers reflect the real production server, which is a Linux server while my machine is
  a Macbook

However, there are still a lot of drawbacks with this approach. Each engineer in the team still has
their own computer with different applications, different setup. We then need to clone the repo,
install all the necessary tools (Nodejs, Homebrew, tmux,…), load the repo into our text editor,
configure the plugins (ex eslint, prettier,…) to work.
- The only things that we can make sure that always behave the same is the VM or the containers
  running inside Docker. Everything else, **No**. It really depends on what is installed on that
  computer. An engineer could have multiple Nodejs versions (installed via nvm) and forget to switch
  to the correct version for the project. An engineer could install a different application on that
  computer, for example, the command like ls behave slighly different on Macos and Linux,…
- We have to configure all the IDE/Editor plugins to follow the correct convention. What will happen
  if we accidentally install a different version of prettier/eslint or install a different version
  of the plugin? What will happen if we misconfigure it?
- As a result, still the classic problem: It works on my machine.
- What will happen if you mess up everything and want to do a fresh setup of the project?
- What will happen if you want to run 2 instances of the same project, to test some configuration?
- How to share a same setup across all team while still maintain the customizability of each
  engineer’s habit?

> Some people also mount the git repo into Docker to run the app directly, but the performance is
> not very good so I won't mention that solution here.

# Adopting Github Codespaces

At Skutopia, we then adopted **Codespaces** as a solution to the above problems. The idea behind is to
transform **Your Computer** into **Somebody else Computer**. Codespaces is a Cloud technology, where
you can quickly spin up a new VM running your whole dev environment. Your computer acts as a UI
only.

[//]: # (The image contains embedded scene for Excalidraw)
![Codespaces](/files/2024-04-19-a-unified-dev-environment/codespaces.png)

Here are the benefits of running Codespaces
- **A single unified development environment** for the whole team, where you define a configuration file
  describing what you want to set up for that machine, even the VSCode plugins.
- You can configure **the exact software** and the version that should be used for the whole team
- You can configure **the exact plugins** to make sure everybody in the company follow the same
  convention
- Once created, the application will **just work**. You can start working immediately, using the
  debugger or running a test case. Everything will be configured already.
- No more **It works on my machine!**
- You can still **customize it**! Simply set a dotfiles repository to install your favorite software.
  It won’t mess up your local computer. You can also override specific VSCode settings if needed,
  for example your favorite key bindings, favorite editor theme, favorite shell setup, etc.
- And of course, fix all the problems mentioned above with previous methods

However, I have never been satisfied enough with Codespaces
- It’s someone else computer (I mean cloud computer), it’s laggy, even if you have a good internet.
  It has been a really annoying feeling for everybody working on Codespaces
- It has limited disk space. The instance we can afford has just 32gb of ram and there is no way
  to increase just the disk space. We have to pay for the higher machine specs. Some of our
  applications consume quite a lot of disk space.
- Its performance is terrible. An M1 macbook with 16gb ram really outperforms a Codespaces instance
  with 4 cores and 16gb ram. We all have M1+ Macbook but cannot utilize that power.
- It’s pricey!

# To Devcontainer

Let’s go into more details about Codespaces

[//]: # (The image contains embedded scene for Excalidraw)
![Codespaces](/files/2024-04-19-a-unified-dev-environment/codespaces-details.png)

- Codespaces is a Linux VM
- It runs Devcontainer, which is actually a Docker container, hence you build it using a Dockerfile
  - Devcontainer is just a wrapper around normal Docker container, providing you some other utilities to make that Docker container a dev environment.
  - Codespaces uses Devcontainer, that's why you see we configure it using devcontainer.json file
- Your VSCode UI doesn't connect to that Codespaces VM. It actually interacts with that Devcontainer. The gh command abstracts everything for you, from starting the VM+container to connect to the correct one.
- Devcontainer is a separate technology. Codespaces just build the wrapper around it to provide the infrastructure. VSCode actually supports us running Devcontainer separately, directly on our computer using Docker.
  - You have already installed this VSCode extension when working with Codespaces. If not, simply install it from VSCode marketplace

[//]: # (The image contains embedded scene for Excalidraw)
![Devcontainer](/files/2024-04-19-a-unified-dev-environment/devcontainer.png)

To get the best of both worlds, fix all the existing problems with Codespaces while still maintaining the benefit of a unified development environment, we should run Devcontainer directly on our machine, not via Codespaces.

> My blog is also configured with Devcontainer now! 😆

# Aren’t we going back to our initial solution?
No. Here is the comparison table

|                                                   | Initial Setup | Codespaces | Devcontainer |
|---------------------------------------------------|---------------|------------|--------------|
| Performant                                        | ✅             | ❌          | ✅            |
| Exactly same environment (apps, plugins, setup,…) | ❌             | ✅          | ✅            |
| Multi instances of the same app                   | ❌             | ✅          | ✅            |
| Easily mess up and rebuild                        | ❌             | ✅          | ✅            |
| Enough disk space                                 | ✅             | ❌          | ✅            |
| Save money                                        | ✅             | ❌          | ✅            |
{: .table }

# Next

- Problems/Solutions when setting up Codespaces/Devcontainer
- Sample Dev workflow with Codespaces/Devcontainer