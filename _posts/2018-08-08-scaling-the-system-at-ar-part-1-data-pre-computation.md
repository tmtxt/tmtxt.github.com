---
layout: post
title: "Scaling the System at AR - Part 1 - Data Pre-Computation"
description: ""
categories: [misc]
thumbnail: /files/ar-logo-1.png
---

At the time of this writing, I have been working at Agency Revolution (AR) for more than 2 years, on
a product focusing mostly on automation email marketing for the Insurance Agencies. I have been
working on this product since it was in beta, when it could only serve only a few clients, send
thousands of emails each month and handle very little amount of integration data without downtime
until it can deliver millions of emails each month, store and react to terabytes of data flow every
day. The dev team has been working very hard and suffering a lot of problem to cope with the
increasing number of customers that the sale team brought to us. Here the summary of some techniques and strategies that we have applied in order to deliver a better user experience.

# The problem of On-demand computing

> By **On-demand**, I mean the action of computing the required data only when it is needed.

One of the core value of our system is to deliver the right messages to the right people at the right time. Our product allows users to set up automated emails, which will be sent at a suitable time in the future. The emails are customised to each specific recipient based on their newest data at the time they receive the email, for example the current customer status, whether that customer is an active or lost customer at that time, how many policies he/she has or the total value that customer has spent until that time.

<!-- more -->

Our system operates based on a queue of all activities that will happen in the future. The system needs to calculate the execution time of all those events to trigger the specific actions targeting the specific set of customers. The emails are also considered as one kind of events in that queue and will be taken out to execute when the time expires.

Our initial implementation simply takes out the events that need to be executed and perform those specific actions. For emails, the task is to take out the specific email from the queue, wrap all the necessary data, from the recipient data, the company information and the email template logic. The system then needs to combine all those information and perform a lot of complex computations based on the email template logic in order to produce the final HTML email content to send to Mailgun. This solution, of course, has many drawbacks. The reason is that there are usually a lot of emails sent at the same time (Our research shows that the customers are more likely to read and click on the email contents if they are sent at some specific time in the day, that’s the business logic, I won’t dive deep in  the details). As a result, when all those emails go out at the same time, there will be a significant decrease in the UI response time because the system will have to spend a lot of resource on processing the outgoing emails. The emails also face a huge delay in delivery time compare to the intial scheduled time. That breaks the core value that we promised to our clients and it is not acceptable.

# Pre-Compute the data when possible

The optimisation turns out to be a very simple idea. The fact is that we already suggested our clients to send the emails at some points during day time, not night time, more specifically in business working hours. Our numbers also show that all of out clients follow that very basic rule. This means there is a waste of system resource since the system mostly stays idle outside of business hours. We decided to utilize that to spread the load evenly between day and night time. The idea is to set up a worker to pre-compute all the necessary data for the next business day (mostly the HTML email content like I mentioned above). The data, of course, is the newest data at the night before the emails go out but the recipient data usually doesn’t change that frequently so it is still ok for us to go with this solution. I will talk about dealing with those problems later. The existing work flow doesn’t need to change a lot. We just need to check whether the pre-computed data for that specific email exists, otherwise, fallback to the old workflow to perform all the complex logic above. The result is amazing and the system can perform incredibly fast even when there thousands of emails going out at that time, which we couldn’t achieve before (for example the Independent day of US).

# Data Invalidation

This optimisation, undoubtedly, brings more complexity to the system (but the benefits are even bigger so it is still a worthy trade-off). Like many other solutions, caching invalidation is still the biggest problem. Luckily, like I said before, the pre-computed emails are stored for maximum 1 day only as we clear all the cached data after it has been used, so usually there are not many items in that table. The solution is simply to create other extra indexes on the cached emails table for better querying. Even if we grow to 1 million emails per day (which is what we are aiming to), it’s still not a big deal for a BTree index to find the necessary item (maximum 20 operations). We created other indexes based on the entities that can impact the output HTML email when they change, like the `companyId`, the `emailTemplateId` or the `recipientId`. Every time one of those entities data changes, we just need to execute a delete query using the index to clear all the related email content.

```js
r.table('cachedEmails')
  .getAll(companyId, { index: 'companyId' })
  .delete();
```

Since those entities usually don’t change very frequently and we have fallback mechanism, the data changes only cause a small performance penalty and it’s quite acceptable. We decided not to update the pre-computed data for simplicity, just delete it and fallback to complex method when required.

If you want, you can read more about RethinkDB index optimisations [here]({%post_url 2020-03-28-scaling-the-system-at-ar-part-2-message-queue-for-integration%}).

# To be continued...
