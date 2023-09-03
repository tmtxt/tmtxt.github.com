---
layout: post
title: "Let's build Product, not Software - Part 2"
description: ""
categories: [misc]
tags: []
thumbnail: /files/2023-08-17-lets-build-product-instead-of-software/img1.png
---

> Previous post: [Let's build Product, not Software - Part 1]({% post_url 2023-08-29-lets-build-product-not-software-part-1 %})

In the first part of this series, I have walked you quickly through some differences
between Building Product and Building Software and why Building Product is important.
In this post, I'll show you a simple example, analyze its problem and come up with
a solution following the Product Engineer mindset.

# A Software Engineer approach

Let's take a look at this simple project

> You are working for an Automation marketing platform for E-commerce merchants.
> The product needs the data about the sales orders of the merchant and your task is to build a
> 1 way Sales Order integration feature to sync data from Shopify to your system

After going through several discovery steps with your customers and the PO, you decide that it's
time to make the implementation plan. You now come up with this plan

Milestones | Details                                                          | Duration
-----------|------------------------------------------------------------------|----------
Backend    | Build the backend to handle auth & webhook requests from Shopify | 1 month
Frontend   | Build the frontend for the users                                 | 1 month
Beta       | Release to some beta customers                                   | 0.5 month
Bug fixes  | Handle issues reported from customers                            | 0.5 month
Go live    | Release to everybody                                             | 0.5 month
{: .table }

Sounds good? Yes, this is a perfect plan from a Software Engineer perspective, but...

<!-- more -->

Assume that it's 01/10/2023 and you have finished the first Backend phase. Everything works as
expected, you bring this to the weekly project update meeting and present to all the stakeholders.
However, ooposite to what is expected, they feel so disappointed. What has happened?
- They have been waiting for 1 month and they still have nothing.
- They have to be patient for another month, for 4 other project update meeting to start using
the feature.
- The PO and QA have also waited for that long, sitting there without doing anything beside
preparing all the test cases

Another month has passes, you now finish the beautiful frontend so everybody can start
experimenting it. The PO now realizes that the workflow is so confusing, could lead to a lot of
complaints from customers. They ask you to go back and change the main workflow. You say
that the business needs to either extend the deadline or accept that flow because it would take
a lot of time to adapt to this change. The whole team start debating because nobody is satisfied.

After several discussions, you have convinced the team to follow the original solution. It's time
to release to some beta customers. What the PO worried about has happened now. The customers say
that the feature is too hard to use and they don't really like this. It's when the project really
go off-track

# How about making smaller milestones?

After reading the above part of this post, you decide to do it differently. You now go back and
update the milestones like this

Milestones                  | Details                                                             | Duration
----------------------------|---------------------------------------------------------------------|---------
Project setup               | Scaffolding, set up server and architecture                         | 1 week
Connect to Shopify          | Connect, authentication and sync first order                        | 2 weeks
Handle Sales order requests | Handle all webhook requests related to Sales orders                 | 1 week
Handle Product requests     | Handle all webhook requests related to Products                     | 1 week
Handle Fulfillment requests | Handle all webhook requests related to Fulfilments                  | 1 week
Handle Customer requests    | Handle all webhook requests related to Customer data                | 1 week
Handle Address requests     | Handle all webhook requests related to Billing and Shipping address | 1 week
Frontend                    | Display the data                                                    | 1 week
Others                      | Implement other small requirements                                  | 1 week
Bug fixes                   | Handle issues reported from customers                               | 2 weeks
Go live                     | Release to everybody                                                | 2 weeks
{: .table }

Sounds better, huh? Yeah this is closer to the target. Smaller milestones make it easier to change.
However, there are still some questions you need to ask here

- Do we look into this from the User perspective? What are the problems that you are trying to solve
for your customers?
- What does it mean when you say `I have finished "Handle Sales order requests" phase`? Does it
mean that you finished setting up the API to handle that type of webhook requests? Or do you
mean that you have completed the function to map from Shopify schema to your system schema? Does
it include the frontend? Can the user start using it?
- Or more precisely, the question is `What is your definition of Done?`. Do you mean it's done when
you say it's done? Do you need to go back and change it if the frontend doesn't fit the data?
- When the project goes off-track and you are going to miss the deadline, what features can you
drop but not affecting the workflow?

> Are you building a Software or a Product?