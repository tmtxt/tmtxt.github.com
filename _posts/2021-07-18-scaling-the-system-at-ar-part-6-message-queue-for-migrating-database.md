---
layout: post
title: "Scaling the System at AR - Part 6 - Message Queue for Migrating Database"
description: ""
categories: [misc]
tags: []
thumbnail: /files/2021-07-18-scaling-the-system-at-ar-part-6/flow.png
---

> Part 5
> [Scaling the System at AR - Part 5 - Message Queue for Scaling team]({%post_url 2021-07-17-scaling-the-system-at-ar-part-5-message-queue-for-scaling-team%})

If you have read some of my previous blog post, you may know that we have been stuck with **Rethinkdb** for
years. **Rethinkdb** was good database. However, the development was stopped some years ago and
there is no sign that it will be continued in the future. We have been following some very active
guys in the community and even thought about donating for them. However, all of them have lost their
interest in **Rethinkdb** and
decided to move forward with other alternative solutions. Also, as I have already mentioned before in
[Mistakes of a Software Engineer - Favor NoSQL over SQL]({&post_url 2021-05-03-mistakes-of-software-developer-favor-nosql-over-sql-part-1&}),
most of our use cases are not suitable with the design of **Rethinkdb** anymore and all the optimizations
that we made are reaching their limit.

After several discussions and analysis, we decided to move away from **Rethinkdb** to
**MS SQL Server**. Some requirements that we have to satisfy are

- The user should be able to view and edit the data normally, without any downtime.
- There should be a backup plan for it.
- There should be an experimental period, where we can pick some users, turn on the new database and
  analyze the correctness of the data.

This can be achieved easily using the Pub/Sub model and Message Queue design described in
[Scaling the System at AR - Part 5 - Message Queue for Scaling team]({%post_url 2021-07-17-scaling-the-system-at-ar-part-5-message-queue-for-scaling-team%}).

![Flow](/files/2021-07-18-scaling-the-system-at-ar-part-6/flow.png)

<!-- more -->

Here are the steps that we followed

- Write to both Databases
  - First, add a new Subscription to the Topic for the Event that you want to migrate. The new
Subscription handler worker is responsible for updating data into the new database (SQL Server in
this case). There is no change need to be made to the existing workflow. After finishing this, the
new data will be presented in both database
- Migrate the data to the new Database
  - You will need to add a migration script to copy over the existing records in the old database to the
new one. Again, there is no change need to be made to the existing workflow and all the existing
functionalities will remain unchanged.
- Turn on the feature
  - At this phase, you have all the data in both places. Your job is simply to turn on the feature flag
for some selected clients (some test users) and verify if the data is correct. You can always revert
back to the old database if you find any problems. When you feel confident, just switch all the
clients to use the new database.
- Clean up
  - When everything is successfully moved to the new database, drop the old Subscription. That's it.
  You don't need to change anything in the current workflow.

Of course, you cannot always use this approach. This solution is based on Pub/Sub model, an
asynchronous design. If you application is implemented in synchronous style, this solution is
probably not for you. Luckily, since we split the team, we have applied this model for most of the
events in our system, to help building small autonomous teams.
