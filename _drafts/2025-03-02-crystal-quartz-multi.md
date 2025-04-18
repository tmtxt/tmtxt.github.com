---
layout: post
title: "CrystalQuartz.Net - Multiple listening addresses"
description: ""
categories: [.net]
tags: []
thumbnail:
---

> [Quartz.NET](https://www.quartz-scheduler.net/) is a full-featured, open source job scheduling
> system that can be used from smallest apps to large scale enterprise systems.

> [Crystal Quartz Panel](https://github.com/guryanovev/CrystalQuartz) is a lightweight, completely
> pluggable module for displaying Quartz.NET scheduler jobs information.

Recently, I was assigned to a project to fix an issue with CrystalQuartz. Due to the lack of
auth features of CrystalQuartz.Net, the team had to set 2 identical instances of
CrystalQuartz on 2 different urls pointing to the same Quartz instance. On top of that, we apply our
own auth layer, which prevents access to certain requests. Specifically, here is how the 2 instances
are created

```csharp
app.UseCrystalQuartz(
    () => scheduler,
    new CrystalQuartzOptions { Path = "/quartz" }
);

app.UseCrystalQuartz(
    () => scheduler,
    new CrystalQuartzOptions { Path = "/auth/quartz" }
);
```

The `/auth/quartz` endpoint is protected with the organization's Windows authentication and allows
certain people to access only. Once you are in, you can execute any requests, including viewing the
dashboard as well as trigger any job. On the other hand, the `/quartz` url is open to everybody in
the company but is limited to this `get_data` request only

```bash
curl 'http://localhost:9000/quartz/' \
  --data-raw 'command=get_data&minEventId=0' \
  ...
```

We then got an issue with the first instance that its progress bars never show up. Although Quartz
is still running, the second instance always show an empty page like this.


![Progress Bars not show](/files/2025-03-02-crystal-quartz-multi/progress-bars-not-show.png)

We tried checking several things around our auth layer but couldn't find anything blocking such
traffic. After digging deeper into CrystalQuartz source code using debugger, I found the real
problem in its event listener logic. CrystalQuartz works by listening to the events from Quartz
instance and build an in-memory data structure corresponding to the current job status so the
frontend can query and display. You can find that logic in this file
[Quartz3SchedulerEngine.cs](https://github.com/guryanovev/CrystalQuartz/blob/5af25443471733e03d01acda5ef86de91ce2c78a/src/CrystalQuartz.Core.Quartz3/Quartz3SchedulerEngine.cs#L52).
Quartz manages the event listener using a name string as the identifier. In this case, you can find
that it uses the `Name` prop of the class `Quartz3SchedulerEventSource`, which is always the
hard-coded string `CrystalQuartzTriggersListener`. Now, it's clear that the second instance
overwrites the event listener of the first one.

We then performed a workaround to make this work, by manually update the name of the
event listener of each instance to a unique one
