---
layout: post
title: "Clean architecture with C#/.Net Core and MediatR - Part 2"
description: ""
categories: []
tags: []
thumbnail:
---

> [Clean architecture with C#/.Net Core and MediatR - Part 1]({post_url 2021-08-09-clean-architecture-with-csharp-net-core-and-mediatr%})

# 2. Adapter Layer

The Business layer mentioned before contains a list of interfaces to connect to other external
dependencies (external services, database storage). It doesn't care what database system to use or
what protocol the external services need. All those logic will be implemented in this
**Adapter layer**.

![Adapter Code](/files/2021-07-13-clean-architecture/adapter-1.png)

An implementation may look like this

```csharp
public class GetContactsByMarketingCampaignId : IGetContactsByMarketingCampaignId
{
    private readonly IMapper _mapper;

    public GetContactsByMarketingCampaignId(IMapper mapper)
    {
        _mapper = mapper;
    }

    public IList<Business.Contact> Execute(int marketingCampaignId)
    {
        // get from Redis cache and then fallback to SQL
        var contacts = GetFromRedis(marketingCampaignId) ?? GetFromSql(marketingCampaignId);

        // use AutoMapper to map back to Business model
        return mapper.Map<IList<Business.Contact>>(contacts);
    }

    private IList<SqlModels.Contact> GetFromRedis(int marketingCampaignId)
    {
        // logic to get from redis here
        ...
    }

    private IList<SqlModels.Contact> GetFromSql(int marketingCampaignId)
    {
        // logic to get from sql here
        ...
    }
}
```

<!-- more -->

Here are something to notice about this layer
- It abstracts all the complex logic to connect to external dependencies. The main business logic
  doesn't have to care whether it is reading from a cache server or a persistent storage. This makes
  testing your main business easier and isolate the cross-cutting concerns.
- You can easily swap the storage layer, introduce any caching layer that you want without affecting
  the main business logic. We applied this to migrate from Rethinkdb to MSSQL database.
- **AutoMapper** is essential. Each layer should work with its own set of models instead of relying on
  the other layer's components.
- It requires integration test effort, where you have to spin up your local database and all the
  related services. Of course, based on the Testing pyramid, there should be fewer test cases here
  compare to the Business layer
  - ![Test Pyramid](/files/2021-07-13-clean-architecture/test-pyramid.png)

You also need to do more setup for testing. Luckily, `Autofac` and `Moq` play nicely together.

```csharp
// XUnit example
public class GetContactsByMarketingCampaignIdTest : IDisposable
{
    private readonly AutoMock _mock;
    private readonly TruongtxSqlContext _dbContext;

    public GetContactsByMarketingCampaignId()
    {
        _mock = AutoMock.GetLoose(builder => builder.RegisterModule<Truongtx.Adapter.AutofacModule>());
        _dbContext = _mock.Create<TruongtxSqlContext>();
    }

    [Fact]
    public async Task FeedbackContentIsNull()
    {
        // Prepare
        var marketingCampaignId = 1;
        var contact = new SqlModels.Contact();
        _dbContext.Contacts.Add(contact);
        _dbContext.SaveChanges();

        // Act
        var handler = _mock.Create<GetContactsByMarketingCampaignId>();
        var res = handler.Execute(marketingCampaignId);

        // Assert
        res.Should()..... // I use FluentAssertion
    }

    public void Dispose()
    {
        // Clean up data here
    }
}
```

# To be continued...
