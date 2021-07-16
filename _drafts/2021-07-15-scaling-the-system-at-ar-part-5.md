---
layout: post
title: "Scaling the System at AR - Part 5 - Message Queue as Publisher/Subscriber pattern"
description: ""
categories: [misc]
tags: []
thumbnail: /files/ar-logo-1.png
---

> Part 4...

# The problem of scaling team

As the product grows bigger and bigger, one team is not capable for developing and maintaining the
whole system anymore. The philosophy that we use for scaling the product is to split it into smaller
ones, building multiple autonomous teams, each team is responsible for one (or more) business
domain. Each individual team is free to experiment new technology and choose the tech stack that is
most suitable for them without impacting the other teams. The organization won't scale if we operate
using a top-down style, where one Production Owner or Tech Leader dictates the decision of all
teams. Sooner or later it will come a big monolith application, where the changes from one place can
cause cascading effect to all the other teams.

I won't talk much about....

For example, the team owning the chat system can choose a database that is more real-time to support
for their purpose. Another team

There are, of course, plenty of difficulties when splitting team

- How do those teams operate independently?
- How can we make sure the error in one team doesn't cause cascading effect to the other team?
- How does one team access the data that another team manages?
- How does one team update the data that belongs to another team?
- How can one team encapsulate the underlying business logic from the other teams?
- How do we maintain the consistency across teams?
- What will happen if we add one (or more team) in the future?
- ...

# How about an API Server for each team?

One simple way that people usually think of is to expose an API Server to the other teams in the
company. An API Server can help

- Limit the permission of other teams/services on the current team resources, allow the other teams
  to read/write certain entities.
- Prevent the other teams to cause unwanted effect on the internal data structure of the current
  team.
- Abstract away the complex underlying business logic of the team

However, this design also comes with a lot problems, more than the benefits that it as

- It has the advantages as well as the limitations of the Synchronous design
  - What happens if the API Server (or one of the internal component of the team) is down? All the
    other teams consuming that API will be blocked. It prevents each individual team from operating
    independently.
  - What happens if there is an error in the API Server logic? All the other teams rely on the data
    returned from the API will be affected. Again, teams are not autonomous anymore.
- In case of querying data, how does one team know which team own the source data?
- In case of updating data, how does one team know which team to update when there is a change?
  - Even if we know which team to update, what API should we call? For example, if there is a new
    entity created in our team, what are the related entities in other teams that we have to update?
    And why do we have to care?
- If we add one (or more) new team in the future, we will have to update code in all teams to make
  sure they update all the other data source when there are any changes.

# Moving to an Asynchronous design

Since the above API design has several problems, we changed to a different design following an
asynchronous style, utilizing our existing infrastructure, to support the ultimate goal of building
autonomous teams.

Here is a brief description of this Asynchronous design
- Each team should has its own database, independent from other teams.
- Each team should store its own data, duplicate the data from the other teams if necessary.
  - In the below picture, with **Customer** entity, each team may need to store the Customer data
    separately to operate on its own. The CRM team needs to store the Customers with all the
    properties so the users can use the CRM system to manage their customer list while the Marketing
    team only to persist the Customers with specific properties to do the automation marketing (only
    `email` and `name` for example).
- The data will eventually be consistent across the system via the global Event system. This is
  built on top of Message Queue design, but it's actually more than that. Behind the scene, our
  system uses Google Cloud Pub/Sub as the backing Message Queue. Luckily, it is more than just a
  simple Message Queue, supports this Pub/Sub model that we need.
  - Each event is represented by a Topic 

![publish-subscribe](/files/2021-07-15-scaling-the-system-at-ar-part-5/publish-subscribe.png)
