---
layout: post
title: "Setup your VPS as a VPN/Proxy server with Pritunl"
description: ""
categories: [misc]
tags: []
thumbnail:
---

I have a computer, which I let it run 24/7 as a home server and I want to access it from anywhere. However,
I want to avoid manual configuring port-forwarding on my home router every time I open a new port on
that server so I set up my VPS as a VPN server, for all of my devices to connect and communicate in
a secure network.

This is actually quite a simple task with `pritunl`. However, some of my friends asked for this so
I posted the instructions here so I can easily share to them.

# Install Pritunl

Before installing `pritunl`, make sure there are no other web server running on your VPS (port `80`
and `443`). You can configure `pritunl` to run on a different port if you are an advanced user
([Pritunl - Load Balancing](https://docs.pritunl.com/docs/load-balancing)).

The installation is straight forward and dead easy ([Pritunl -
Installation](https://docs.pritunl.com/docs/installation)).
Basically, what you need to do is just to run the commands specified in that doc. For
`Ubuntu 18.04`, it would be

This is **1** command
```shell
sudo tee /etc/apt/sources.list.d/mongodb-org-4.0.list << EOF
deb https://repo.mongodb.org/apt/ubuntu bionic/mongodb-org/4.0 multiverse
EOF
```

This is **1** command
```shell
sudo tee /etc/apt/sources.list.d/pritunl.list << EOF
deb http://repo.pritunl.com/stable/apt bionic main
EOF
```

This is **6** commands
```shell
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com --recv 9DA31620334BD75D9DCB49F368818C72E52529D4
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com --recv 7568D9BB55FF9E5287D586017AE645C0CF8E292A
sudo apt-get update
sudo apt-get --assume-yes install pritunl mongodb-server
sudo systemctl start pritunl mongodb
sudo systemctl enable pritunl mongodb
```

Also, run these commands after finishing the installation to increase the open file limit

```shell
sudo sh -c 'echo "* hard nofile 64000" >> /etc/security/limits.conf'
sudo sh -c 'echo "* soft nofile 64000" >> /etc/security/limits.conf'
sudo sh -c 'echo "root hard nofile 64000" >> /etc/security/limits.conf'
sudo sh -c 'echo "root soft nofile 64000" >> /etc/security/limits.conf'
```

# First Configuration

Open your browser, head to `https://your-ip`, confirm the Security Exception (because it is using a
self-signed certificate). You can always add your own certificate later (using **Letsencrypt** or any
other certificate provider).g

It will prompt for the **setup key**

![Setup Key](/files/2019-03-17-setup-your-vps-as-a-vpnproxy-server-with-pritunl/p1.png)

Go back to your VPS, run the command shown on the screen to get the **setup key** and proceed to
next step.

```shell
sudo pritunl setup-key
```

![Setting up](/files/2019-03-17-setup-your-vps-as-a-vpnproxy-server-with-pritunl/p2.png)

Now it will prompt you for the login credential. Again, go back to your VPS console and run the
command shown on the screen

```shell
$ sudo pritunl default-password
[2019-03-17 07:06:24,907][INFO] Getting default administrator password
Administrator default password:
  username: "pritunl"
  password: "xxx"
```

Use that information to login. You can the default password immediately after first login.

# Set up Organization & User

Click on the `Users` tab on the top of the screen.

Click on the `Add Organization` button, enter the organization name and click `Add`.

![Setting up](/files/2019-03-17-setup-your-vps-as-a-vpnproxy-server-with-pritunl/p4.png)

Continue with the `Add User` button, enter the username, select the organization you've created,
enter the secret PIN and click `Add`.

![Setting up](/files/2019-03-17-setup-your-vps-as-a-vpnproxy-server-with-pritunl/p5.png)

# Set up the Server

You need to create a server so the user can connect from their device to.

Click on the `Servers` tab on the top of the screen.

Click on the `Add Server` button.

* Enter the `Name` for the server.
* Select a `Port` for the server to run. You can keep the auto-generated port. Make sure it doesn't
  conflict with any other application running on that server and your firewall doesn't block that
  port.
* Keep the `Protocol` and `DNS Server` unchanged.
* The `Virtual Network` will determine the ip range and the number of clients can connect to this
  server. For example, if the value is `192.168.221.0/24`, the client connecting to this server will
  be assigned the ip `192.168.221.2`, `192.168.221.3`, `192.168.221.4`,... You can keep this value
  unchanged, just need to take note so later we know the ip to connect.
* You can optionally enable Two-Step Authentication on this server (you will need the authentication
  app like **Google Auth** or **Authy** installed on your phone).

![Setting up](/files/2019-03-17-setup-your-vps-as-a-vpnproxy-server-with-pritunl/p6.png)

Click on `Attach Organization` to attach your organization to the new server.

Everything is ready! Now click `Start Server` to make the server go live.
