---
layout: post
title: "Some Optimizations in RethinkDB - Part 2"
description: ""
categories: [rethinkdb]
tags: [rethinkdb]
thumbnail: /files/2017-10-08-utilize-rethinkdb-index-part-1-primary-key-index/thumbnail.png
---

**Part 1 here**.

> Yes, it's RethinkDB, a discontinued product. Again, read my introduction in the previous post.
> It's not only about RethinkDB but it also the basic idea for many other database systems. This post introduces other techniques that I and the have applied at AR to maximize the workload that RethinkDB can handle but most of them can be applied for other database systems as well.

# Increase the Memory with NVME SSD

This is applicable for the ones that are running on cloud services that support this kind of SSD interface only. 

# Quick flush vs Late flush

# Temporary data, scale with K8S

> This is a trick, work around not only for RethinkDB but also all the other database systems.

Since we are running the application inside Docker, deployed in Kubernetes on Google Cloud, we have free auto-scale ability from Google. What does this mean in scaling and optimising the database? Application scaling and Database are 2 different aspects. All the shared state should be stored in a backing service and scale differently. The application inside Docker itself should be as stateless as possible and should be safe to be completely destroyed. The containers are designed to be scale up and down in this way. It’s quite hard and not practical to scale the database using k8s (although there are database designed to be scaled with k8s recently). But well, there will be the case that all you need is to store a large amount of data temporarily for a shot period of time and K8S pods would be a very suitable choice for this case.

We faced this situtation before. The requirement is to process a large number of records from an input XLSX file. Each record data can be spread across multiple rows (because it contains some array props) in the Excel file and the data can be duplicated. The size of the file can be up to thousand to million of rows. It’s not possible to put read and process everything in memory (of course). We also need a way to join related data belong to one records as well as remove duplicated data. Any database table with the primary key index can solve this problem easily. Everytime you finish reading one line in the file, execute a write or update (upsert) query with the corresponding primary key to store the data into the database to process later.

For the above requirement, we ran into the case that the temporary data slow down the whole system due to high read/write throughput. The whole system was slowed down because of those temporary data processing. That's not acceptable because the temporary data that the users never touch impacts the user experience of the whole system. For this case, usually we will send a message to Google PubSub (or Kafka in your case) and schedule a k8s pod running as a background job to process and convert the data into something useful that we can use. This is where k8s and its auto-scaler shine. Instead of running just 1 container inside 1 pod as usual, we can configure 1 container for the application and one other for the RethinkDB instance for storing the temporary data. The cool thing is that if you schedule multiple pods running in parallel, Google Cloud k8s can scale them automatically by adding more instance or adding more worker nodes to handle the work load. You probably don't even need to care about controlling the read/write speed. All you need to do is to control how it write to your main database to persist the data but of course, batch and sequential writing only are much faster and reading, writing and updating at the same time and also easier to control.
