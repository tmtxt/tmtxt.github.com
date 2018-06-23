---
layout: post
title: "Some Optimizations in RethinkDB - Part 2"
description: ""
categories: [rethinkdb]
tags: [rethinkdb]
thumbnail: /files/2017-10-08-utilize-rethinkdb-index-part-1-primary-key-index/thumbnail.png
---

**Part 1 here** [Some Optimizations in RethinkDB - Part 1]({%post_url 2018-03-10-some-optimizations-in-rethinkdb-part-1%})

> Yes, it's RethinkDB, a discontinued product. Again, read my introduction in the previous post.
> It's not only about RethinkDB but it also the basic idea for many other database systems. This
> post introduces other techniques that I and the team have applied at AR to maximize the workload that
> RethinkDB can handle but most of them can be applied for other database systems as well.

# Increase the Memory with NVME SSD

Well, sound like a very straight forward solution, huh? More memory, better performance, sound quite
obvious! Yes, the key thing is how to increase the memory without significant cost. The answer is to
setup swap as the temporary space for storing RethinkDB cached data. RethinkDB, as well as other
database systems, caches the query result data into memory so that it can be re-used next time the
same query executes again. The problem is that swap is much slower than RAM, because we rely on
the disk to store the data. However, since we are running on Google Cloud and Google Cloud offers
the **Local-SSDs** solution, we have been exploiting this to place our swap data. Here is the
**Local-SSDs** definition, according to Google

> Local SSDs are physically attached to the server that hosts your virtual machine instance. Local
> SSDs have higher throughput and lower latency than standard persistent disks or SSD persistent
> disks. The data that you store on a local SSD persists only until the instance is stopped or
> deleted.

<!-- more -->

Sound like a perfect candidate for storing data like swap, isn't it? Google Cloud
**Local-SSDs** are the best fit for storing the temporary data that requires high performance and
read/write throughput. The **Local-SSD** instances are attached directly to your server, not through
any middle layers for replicating data so that it gains the best performance. The biggest limitation
of **Local-SSDs** is that the data is not persistent, all the data will be lost once the VM instance
restarts, but it's just the swap data so we don't even bother losing it. After doing all the below
optimizations, we ended up with a VM instance with 400-500GB swap data, about 4 times bigger and 4 times
slower than the VM memory with much lower cost than upgrading the VM memory. That's quite
acceptable for us to run many of our RethinkDB instances without any bottleneck. Here are the steps
that we had applied

- Choose a cloud provider that supports this kind of SSD interface and offers an optimized OS image for it. Luckily, Google Cloud already has this option for us. For other cloud services, I think there will be similar alternative solutions.
- Optionally, you can configure RAID if you have multiple **Local-SSDs** attached to your server. Google has a brief tutorial [Adding Local SSDs](https://cloud.google.com/compute/docs/disks/local-ssd), the **Format and mount multiple local SSD devices into a single logical volume** section.
  - Configuring `RAID 0` can help increase the read and write speed but decrease the durability. One fail disk can lead to the whole system failure.
  - Due to the fact that **Local-SSDs** are attached directly to the VM, without any replication layer, running any RAID configurations other than `RAID 0` (like `RAID 1` or `RAID 10`) doesn't provide any significant benefits
- You may need some optimisations depending on the SSD interface that you are using. This [post](https://cloud.google.com/compute/docs/disks/performance) gives some instructions on how to optimise **Local-SSDs** on Google Cloud.
- You can also setup a RAM disk, [benchmark](https://cloud.google.com/compute/docs/disks/performance) and compare the performance to choose the best optimisation. We achieved the Local SSD speed ~1/4 of the RAM disk.
- Of course, you need to set up swap space for your server. There are plenty of guides out there on the internet, for example this one [How To Add Swap Space on Ubuntu 16.04](https://www.digitalocean.com/community/tutorials/how-to-add-swap-space-on-ubuntu-16-04)
- Finally, configure the memory allocation for RethinkDB instance if you are running inside docker.
- Extra: you may have to prepare some scripts to restore the server in case of failure because swap configuration and the data on the **Local-SSDs** will be lost in case of failure.

RethinkDB may sometimes complain about the data being placed on swap memory but that's our purpose for doing this. Since we migrated to Google Cloud and applied this caching strategy, our RethinkDB clusters performance increased dramatically because we had more space for caching the data to run large analytic queries.

**Note**: currently Google is having some issues with the **NVME Local SSDs** and sometimes make the data on the **Local SSDs** corrupted. Google is working on it and they promised it will be fixed in Q3 2018.

# Temporary data, scale with K8S

> Again, this trick/workaround works not only for **RethinkDB** but also for all the other database systems.

Because we are running our application inside Docker, deployed in Kubernetes on Google Cloud, we have
free auto-scale ability from Google. What does this mean in scaling and optimising the database?
Application scaling and Database scaling are 2 different aspects. All the shared state should be stored in a
backing service and scale differently. The application inside Docker itself should be as stateless
as possible and should be safe to be completely destroyed. The containers are designed to be scale
up and down in this way. It’s quite hard and not practical to scale the database using k8s (although
there are databases designed to be scaled with k8s recently). But well, there will be the case that
all you need is to store a large amount of data temporarily for a shot period of time and K8S pods
would be a very suitable choice for this case.

We faced this situation before. The requirement is to process a large number of records from an
input XLSX file. Each record can be spread across multiple rows (because it contains some array
props) in the Excel file and the data can be duplicated. The size of the file can be up to thousand
to million rows. It’s not possible to read and process everything in memory (of course). We
also need a way to join related data belong to one record as well as remove duplicated data. Any
database table with the primary key index can solve this problem easily. Everytime you finish
reading one line in the file, execute a write or update (upsert) query with the corresponding
primary key to store the data into the database to process later.

For the above requirement, we ran into the case that the temporary data slow down the whole system
due to high read/write throughput. The whole system was stressed because of the temporary data
processing. That's not acceptable because the temporary data that the users never touch impacts the
user experience of the whole system. For this case, usually we will send a message to Google PubSub
(or Kafka in your case) and schedule a k8s pod running as a background job to process and convert
the data into something useful that we can use. This is where k8s and its auto-scaler shine. Instead
of running just 1 container inside 1 pod as usual, we can configure 1 container for the application
and one other for the RethinkDB instance for storing the temporary data. The cool thing is that if
you schedule multiple pods running in parallel, Google Cloud k8s can scale them automatically by
adding more instances or adding more worker nodes to handle the work load. You probably don't even
need to care about controlling the read/write speed. All you need to do is to control how it write
to your main database to persist the data but of course, batch and sequential writing only are much
faster and reading, writing and updating at the same time and also easier to control.

# Wait for RebirthDB

This is actually not an optimisation :LOL:. It's just something that we are looking forward to seeing soon. **RebirthDB** is a community-driven fork of **RethinkDB** that comes with some optimisations that you can configure like Data flushing strategy. That will hopefully allow you to manage the instance's memory more efficiently depending on the specific use case. You can choose whether to keep the recently-written data in memory for caching or to flush them to disk immediately to save caching memory for other critical data.

I don't really like working with NoSQL but the fact is that our product is running based on **RethinkDB** so I'm very glad to see that someone on the community still keep developing this.