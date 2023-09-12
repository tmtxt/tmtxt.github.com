---
layout: post
title: "Let's build Product, not Software - Part 3"
description: "In the previous post, I have shown you a real example about how to solve the problem as a Software Engineer. This time, we gonna do it by the Product Engineer approach."
categories: [misc]
tags: []
thumbnail: /files/2023-08-17-lets-build-product-instead-of-software/delivery-1.png
---

[Let's build Product, not Software - Part 2]({% post_url 2023-09-03-lets-build-product-not-software-part-2 %})

In the previous post, I have shown you a real example about how to solve the problem as a
Software Engineer. This time, we gonna do it by the Product Engineer approach.

> Make Agile great again!

If you refer to the first post that I wrote, Building Product is about delivering user values,
collecting feedbacks and constantly adapting to the change of business. Does it sound like Agile?
Yes, in my opinion, Agile seems to be the best fit out there for Product company at a small
and medium size.

# Don't do this

Let's start with the non-Agile way by this picture-by-picture story

![delivery-1](/files/2023-08-17-lets-build-product-instead-of-software/delivery-1.png)

![delivery-2](/files/2023-08-17-lets-build-product-instead-of-software/delivery-2.png)

![delivery-4](/files/2023-08-17-lets-build-product-instead-of-software/delivery-4.png)

![delivery-3](/files/2023-08-17-lets-build-product-instead-of-software/delivery-3.png)

When you put this into a Software perspective, it's pretty much the same with the
[first example]({% post_url 2023-09-03-lets-build-product-not-software-part-2 %}#a-software-engineer-approach)
that I showed in my previous post. The way most people would choose is to implement the whole
feature, from backend to frontend before delivering to the customer. Again, do NOT do this.

<!-- more -->

# but use this Incremental Delivery model

![delivery-5](/files/2023-08-17-lets-build-product-instead-of-software/delivery-5.png)

How about taking 1 parcel at a time? Begin with the most valuable one and repeat until you're done?

Does it feel repetitive? Yes, but it's less risky because it's much simpler for you to implement.
You can ship the values continuously, collecting the feedbacks from your customers. When there are
any problems, you can quickly change the route (the approach). Sometimes, you can even throw away
the unnecessary features based on the feedbacks. By starting with the most valuable ones, you also
focus on resolving 80% of the problem using 20% of the effort, gain the most revenue for
the company.

The Agile way                       | The old way
------------------------------------|---------------------------------------
Deliver values continuously         | Wait until the end to see the result
Keep Customers in a feedback loop   | Cannot give feedbacks until very late
Less multi-tasking                  | Many features at once
Less big work, easier to refactor   | Hard to make it smaller
Can throw away unnecessary features | Don't know what to throw until the end
{: .table }

# Start from User Values

Ok. If all the above sound so theoretical, how about taking the Integration project that I
mentioned in
[part 2]({% post_url 2023-09-03-lets-build-product-not-software-part-2 %})
of this series and do it the Agile way?

Instead of defining the tickets/milestones based on the code implementation, try re-writing the
Acceptance Criteria following this format

```
- As a user (a staff, a system admin,...), I want...
- (Optional) so that I can solve (don't have to, be able to,...) ...
```

- As a user of XXX, I want to be able to log in to Shopify from XXX so I can verify
that the credentials are correct.
- As a user of XXX, I want my order to be synced to XXX when it is placed on Shopify.
- As a user of XXX, I want my order to be updated in XXX when it is changed on Shopify.
- As a user of XXX, I want my fulfillment information to be reflected in XXX when I mark the order
as fulfilled on Shopify.

# then come up with the implementation

For instance, you have this AC from the above

> As a user of XXX, I want my order to be synced to XXX when it is placed on Shopify.

you will then define the steps to execute it
- Set up the webhook endpoint to handle `OrderCreated` event from Shopify.
- Configure Shopify to send updated data to the new webhook endpoint.
- Implement the mapping logic to map from Shopify sales order to XXX sales order schema.
- Add a new API to get list of orders (very basic information).
- Design a **simple** UI so the user can verify the information.

We should try to make this as simple and as achievable as possible. This ticket should not target
on building a beautiful UI. It should follows exactly the pre-defined AC and provide a very simple
UI to verify the data only. After that, there should be another ticket to enhance the UI.

# Defining User Value rules

Here are some rules for you to follow to ensure that the project will be on the right track

- Usually, you still need a scaffolding phase in the beginning.
  - Again, be agile, try to make it as small as possible. Don’t try to be perfect!
- A User value **isn’t necessarily a big and complete feature**.
  - If it’s still too big, split it until it’s small enough for an `S`, `M` or maximum `L` estimate.
  Never create `XL`+ ticket
- However, a User value/ticket should be **a complete flow**, frontend to backend
  - That means the Users can use it
- Be clear about the target users and decide whether this feature is necessary for that target users.
- A clear definition of **Done**
  - Done is when the feature flows all the way to Production and is shipped to the user. You don't
  have to come back anymore
- Do **NOT** include future work.
- Prioritization: be **80-20**, highest value with smallest effort first.

![prioritization](/files/2023-08-17-lets-build-product-instead-of-software/prioritization.png)

# To be continued

More real world examples...