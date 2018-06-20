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
> It's not only about RethinkDB but it also the basic idea for many other database systems.

# Increase the Memory

Hmm, sound like a very obvious solution?

# Quick flush vs Late flush 

# Temporary data, scale with K8S

> This is a trick, work around not only for RethinkDB but also all the other database systems.

Since we are running the application inside Docker, deployed in Kubernetes on Google Cloud, we have free auto-scale ability from Google. What does this mean in scaling and optimising the database? Application scaling and Database are 2 different aspects. All the shared state should be stored in a backing service and scale differently. The application inside Docker itself should be as stateless as possible and should be safe to be completely destroyed. The containers are designed to be scale up and down in this way. It’s quite hard and not practical to scale the database using k8s (although there are database designed to be scaled with k8s recently). But well, there will be the case that all you need is to store a large amount of data temporarily for a shot period of time and K8S pods would be a very suitable choice for this case.

We ran into the case that the temporary data slow down the whole system due to high read/write throughput. The requirement is to process a large number of records from an input XLSX file. Each record data can be spread across multiple rows (because it contains some array props) in the Excel file and the data can be duplicated. The size of the file can be up to thousand to million of rows. It’s not possible to put read and process everything in memory (of course). We also need a way to join related data belong to one records as well as remove duplicated data. Any database table with the primary key index can solve this problem easily. Everytime you finish reading one line in the file, execute a write or update (upsert) query with the corresponding primary key to store the data into the database to process later.