---
layout: post
title: "Configuration in .Net Core"
description: ""
categories: [misc]
tags: []
thumbnail:
---

> `Configuration`, it sounds very simple but many people do it the wrong way.

**Configuration** is an essential part of any application (The term **Configuration** here does not
include internal application config, such as the route path definition). **Configuration** varies
across deploys and environments, which provides confidential information to the application (such as
database connection string) and defines the way the application should behave (for example, enable or
disable specific features). It sounds quite simple but I found that many people did it the wrong way. That’s why we have this blog post.

# Take a look at this

Recently, I worked on another project, which was written in **ASP.Net Core 3.1**. After a while
digging into the code, I saw these patterns and felt really annoyed about it. They are related to
the way the **Configuration** is implemented and passed around the application as well as the way it
is consumed.

Here is the `Setting` interface and its implementations.

```csharp
public interface ISetting
{
    Env Environment { get; }
    string MailgunApiKey { get; }
    string AppName { get; }
}

public class LocalSetting : ISetting
{
    Env Environment => Env.Local;
    public string MailgunApiKey => "key-xxxx";
    public string AppName => "Prod Name";
}

public class StagingSetting : ISetting
{
    Env Environment => Env.Staging;
    public string MailgunApiKey => "key-yyyy";
    public string AppName => "Prod Name";
}

public class ProdSetting : ISetting
{
    Env Environment => Env.Prod;
    public string MailgunApiKey => "key-zzzz";
    public string AppName => "Prod Name";
}
```

Here is an app component which uses the **Setting** object

```csharp
public async Task Execute()
{
    var setting = Resolve<ISetting>();
    var emailToUse = "";

    switch (setting.Environment)
    {
        case Env.Local:
            emailToUse = "email.local@mydomain.com";
            break;
        case Env.Staging:
            emailToUse = "email.stage@mydomain.com";
            break;
        case Env.Prod:
            emailToUse = request.ActualEmailAddress;
            break;
        default:
            break;
    }

    await SendEmail(emailToUse, setting.MailgunApiKey);
}
```

<!-- more -->

# When Configuration gets wrong

> A litmus test for whether an app has all config correctly factored out of the code is whether the
> codebase could be made open source at any moment, without compromising any credentials.
> [The Twelve Factor App](https://12factor.net/config)

You can spot a lot of problems with the above code. Some are quite obvious (like the hard-code
values), some aren't. Let's take a look at each of them again and analyze

First, start with the code to define the `ISetting` interface

```csharp
public interface ISetting
{
    Env Environment { get; }
    string MailgunApiKey { get; }
    string AppName { get; }
}

public class LocalSetting : ISetting
{
    Env Environment => Env.Local;
    public string MailgunApiKey => "key-xxxx";
    public string AppName => "Local Name";
}

public class StagingSetting : ISetting
{
    Env Environment => Env.Staging;
    public string MailgunApiKey => "key-yyyy";
    public string AppName => "Staging Name";
}

public class ProdSetting : ISetting
{
    Env Environment => Env.Prod;
    public string MailgunApiKey => "key-zzzz";
    public string AppName => "Prod Name";
}
```

What are the problems here?

- Most obvious one, the hard-coded credentials. These things should not be stored in source
  control. What happen if a new guy join the team and he accidentally start the app with **Prod** as
  the default environment?
- As a **.Net (Core)** developer, especially **ASP.Net (Core)** developer, this is confusing for me. It
  increases the complexity of the application in an unnecessary way. Why? Microsoft have already
  provided a standard for storing and loading hard-coded values via the `appsettings.json`
  file. Why do we have to make it more complex? Why do we have to invent another method for that
  purpose? The first time when I read the code, I saw all these classes along with other
  `appsettings.json` files and I 😳
- It is not easy to update the configuration values. To change a single value, you will need to
  update your code, recompile it and trigger a redeployment of the app, which can take a lot of
  time. A better way should be to store them in environment variables, change the environment
  variables and then reload your app to run with the new values. Read more about `Config` in
  `12factor` app [here](https://12factor.net/config).

OK, coming to the next part

```csharp
public async Task Execute()
{
    var config = Resolve<IConfiguration>();
    var emailToUse = "";

    switch (config.Environment)
    {
        case Env.Local:
            emailToUse = "email.local@mydomain.com";
            break;
        case Env.Staging:
            emailToUse = "email.stage@mydomain.com";
            break;
        case Env.Prod:
            emailToUse = request.ActualEmailAddress;
            break;
        default:
            break;
    }

    await SendEmail(emailToUse, config.MailgunApiKey);
}
```

The above code piece tries to protect the developers from accidentally sending a test email to a real
customer in Local or Staging environment by replacing the input email with an internal one.

However...

- It is limited to just 3 environments: **Local**, **Staging** and **Prod**. What if you want to add
  one more environment, perhaps another **Staging** environment for a new team? You will have scan
  through your code base, find all the places using `swtich`/`case` like this and update them. No,
  it's not a
  good idea at all. We had this problem before when the Vietnam team started working on an existing
  project of the US team and we had to set up a new environment for the Vietnam dev team, things
  were messed up as we couldn't scan and update every single place in code.
- The business logic is mixed with the **Configuration** while they should be separated.
  - The code should have no realization of the environment it is running on so it is easy to scale
  the app, to add more instances running in different architecture. This could be applied when you
  want to add a new temporary server to cope with the high
  workload or to add another server, which reflects the production server (with specific features disabled) for testing purpose.
  - This also exposes the problem of **Cross-cutting concerns**. Each component should be as simple
    as possible, it does only what it should. All the other **cross-cutting concerns** should
    be decomposed from the main component so it is easy to do unit testing.

# Let's fix it

So, what are the requirements?

- Configuration should be separated from code logic. The code should just focus on business
logic and should not contain any of the logic to check for specific environment (like all the
`switch`/`case` we saw above).
- We should be able to override any specific value seamlessly. It should allow us to enable or
  disable any feature easily by turning on or off some environment variables rather than scanning
  through all places in the code to change manually.
- It should be easy to add new or switch to another environment.
- Of course, environment types and credentials should not be hard-coded in code.

Here is the sample flow of building the **Configuration** instance

![Flow](/files/2020-04-07-stop-doing-this-you-have-a-better-way-to-write-config-in-net-core/flow.png)

1. All the setting values are loaded from different locations, through several layers of override that we
   defined. This is the normal way that you would do with
   [ASP.Net Core](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration/?view=aspnetcore-3.1).
   The final result would be an instance of `IConfiguration`, which is a key-value dictionary of all
   the raw setting values. This allows us to switch the settings very easily by changing the input
   files or just some specific environment variables.
2. For simplicity in usage, the `IConfiguration` instance will be injected into another layer of
   custom `AppConfig` object, where you can build, validate and transform all the raw string values to
   the correct format that you want.
3. The App components then consume the configuration simply by resolving the `AppConfig` instance
   from the DI Container.

## 1. Load Configuration values

Microsoft has already documented and explained all the steps to build and register the
`IConfiguration` implementation with DI Container here
[Configuration in ASP.NET Core](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration/?view=aspnetcore-3.1).
Basically, what you need to do is just specify the load order when creating `IHostBuilder` instance.

```csharp
Host.CreateDefaultBuilder(args)
    .ConfigureAppConfiguration((hostingContext, config) =>
    {
        var environmentName = hostingContext.HostingEnvironment.EnvironmentName;
        // Use this for hard-coded values that are not confidential
        config.AddJsonFile($"appsettings.{environmentName}.json", optional: true);
        config.AddJsonFile($"appsettings.json", optional: true);
        // Use this for credentials running on Prod
        config.AddEnvironmentVariables();
    })
```

If you are writing your own Console application (or other types of application which are not
**ASP.Net Core**), simply install these extensions on **Nuget**

- Microsoft.Extensions.Configuration
- Microsoft.Extensions.Configuration.Json
- Microsoft.Extensions.Configuration.FileExtensions
- Microsoft.Extensions.Configuration.EnvironmentVariables

Here is a simple example showing how to register it with **Autofac Module**

```csharp
protected override void Load(ContainerBuilder builder)
{
    builder.Register(f =>
    {
        var environmentName =
            Environment.GetEnvironmentVariable("ASPNETCORE_ENVIRONMENT");

        var config = new ConfigurationBuilder();
        config
            .AddJsonFile($"appsettings.{environmentName}.json", optional: true)
            .AddJsonFile($"appsettings.json", optional: true)
            .AddEnvironmentVariables();

        return config.Build();
    }).As<IConfiguration>();
}
```

## 2. Transform raw string values

The preferred way is to load Config values from environment variables. That means, all the raw
values should be of type string. For simplicity in later usage, it is recommended to add another
layer of `AppConfig`, where you can validate and transform the raw string values to the correct
types that you expect. This can be easily achieved using extension methods.

```csharp
public static string GetRequiredString(this IConfiguration configuration, string key)
{
  var val = configuration[key];
  if (!string.IsNullOrWhiteSpace(val))
    return val;

  throw new RequiredValueMissingException(key);
}

public static int GetInt(this IConfiguration configuration, string key, int defaultValue)
{
  var val = configuration[key];
  if (string.IsNullOrWhiteSpace(val))
    return defaultValue;

  return Convert.ToInt32(val);
}
```

```csharp
public class AppConfig
{
    public string LogLevel { get; set; }
    public string MailgunApiKey { get; set; }
    public bool SendRealEmail { get; set; }
    public string DefaultToEmail { get; set; }

    public AppConfig(IConfiguration config)
    {
        LogLevel = config.GetString("LOG_LEVEL", defaultValue: "Info");

        // validate this is required
        MailgunApiKey = config.GetRequiredString("MAILGUN_API_KEY");

        // to be safe, default value for this should be false
        SendRealEmail = config.GetBool("SEND_REAL_EMAIL", defaultValue: false);
        DefaultToEmail = config.GetString(
            "DEFAULT_TO_EMAIL", defaultValue: "test@test.test"
        );
    }
}
```

And here is how to register it with **Autofac**

```csharp
protected override void Load(ContainerBuilder builder)
{
    // register directly
    builder.RegisterType<AppConfig>();

    // or if you want an interface
    builder.RegisterType<AppConfig>().As<IAppConfig>();
}
```

## 3. Consume the AppConfig object

Finally, to consume `AppConfig`, simply resolve it from your DI Container.

```csharp
public class SendEmail
{
    public async Task Execute(string email)
    {
        var config = scope.Resolve<AppConfig>();

        // whether to send real email?
        var sendRealEmail = config.SendRealEmail;
        if (!sendRealEmail) email = config.DefaultToEmail;

        //
        await mailgunClient.Send(email);
    }
}
```

The main component logic is now extremely simple and very easy to write unit test. It doesn't care
about the current environment that it is running at all, no matter if you are running a Console
application, a Docker container in your local computer or a Docker container on your
Production Kubernetes cluster. To add a new environment or a new server with some features disabled,
simple override the necessary environment variables, there is no need to update the code and rebuild
any component. In case of error and you want to disable the email feature on your Prod, what you need to is
also just to update an environment variable and reload your application.

# Hmmmm...

This is easy, this is simple, there is a standard solution from **Microsoft** and
[12factor App](https://12factor.net/config), so

- Do it the right way!
- Don't reinvent the wheel! (unless you have a really good reason)
