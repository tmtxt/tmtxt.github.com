---
layout: post
title: "Scaling the System at AR - Part 5 - Message Queue for Cross Team Communication and Database Migration"
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
teams. Sooner or later it will come a big

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

- This is a synchronous design.
  - What happens if the API Server (or one of the internal component of the team) is down? All the
    other teams consuming that API will be blocked. It prevents each individual team from operating
    independently.
  - What happens if there is an error in the API Server logic? All the other teams rely on the data
    returned from the API will be affected. Again, teams are not autonomous anymore.
- In case of querying data, how does one team know which team own the source data?
- In case of updating data, how does one team know which team to update when there is a change?
- If we add one (or more) new team in the future, we will have to update code in all teams to make
  sure they update all the other data source when there are any changes.
