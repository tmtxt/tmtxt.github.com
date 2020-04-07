---
layout: post
title: "Stop doing this! You have a better way to write Config in .Net Core"
description: ""
categories: [misc]
tags: []
thumbnail:
---

> `Configuration`! It seems to be very simple but many people can easily implement it in a wrong way.

# When Configuration gets wrong

So, recently, I saw these patterns in the code and feel really annoyed about it

Here is the `Configuration` interface and its implementations.
{% highlight csharp %}
public interface IConfiguration
{
    Environment env { get; }
    string MailgunApiKey { get; }
    // ... other props
}

public class LocalConfiguration : IConfiguration
{
    Environment env => "local";
    public string MailgunApiKey => "key-xxxx";
    // ... other props
}

public class UatConfiguration : IConfiguration
{
    Environment env => "uat";
    public string MailgunApiKey => "key-yyyy";
    // ... other props
}

public class ProdConfiguration : IConfiguration
{
    Environment env => "prod";
    public string MailgunApiKey => "key-zzzz";
    // ... other props
}
{% endhighlight %}
