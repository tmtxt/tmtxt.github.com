---
layout: post
title: "Stop doing this! You have a better way to write Config in .Net Core"
description: ""
categories: [misc]
tags: []
thumbnail:
---

> `Configuration`! It seems to be very simple but many people can easily implement it in a wrong way.

So, recently, I saw these patterns in the code and feel really annoyed about it

Here is the `Configuration` interface and its implementations.
{% highlight csharp %}
public interface IConfiguration
{
    Env Environment { get; }
    string MailgunApiKey { get; }
    string AppName { get; }
}

public class LocalConfiguration : IConfiguration
{
    Env Environment => Env.Local;
    public string MailgunApiKey => "key-xxxx";
    public string AppName => "Prod Name";
}

public class StagingConfiguration : IConfiguration
{
    Env Environment => Env.Staging;
    public string MailgunApiKey => "key-yyyy";
    public string AppName => "Prod Name";
}

public class ProdConfiguration : IConfiguration
{
    Env Environment => Env.Prod;
    public string MailgunApiKey => "key-zzzz";
    public string AppName => "Prod Name";
}
{% endhighlight %}

Here is how to consume it
{% highlight csharp %}
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
{% endhighlight %}

# When Configuration gets wrong

You will spot a lot of problems in the above code. Some are quite obvious (like the hard-code
values), some aren't. Let's take a look at each of them again and analyze

First, start with the code to define the Config instance
{% highlight csharp %}
public interface IConfiguration
{
    Env Environment { get; }
    string MailgunApiKey { get; }
    string AppName { get; }
}

public class LocalConfiguration : IConfiguration
{
    Env Environment => Env.Local;
    public string MailgunApiKey => "key-xxxx";
    public string AppName => "Local Name";
}

public class StagingConfiguration : IConfiguration
{
    Env Environment => Env.Staging;
    public string MailgunApiKey => "key-yyyy";
    public string AppName => "Staging Name";
}

public class ProdConfiguration : IConfiguration
{
    Env Environment => Env.Prod;
    public string MailgunApiKey => "key-zzzz";
    public string AppName => "Prod Name";
}
{% endhighlight %}

What are the problems here?

- Most obvious problem, the hard-coded credentials. These things should not be stored in source
  control. What happen if a new guy join the team and he accidentally start the app with **Prod** as
  the default environment? Will things be messed up? You never know.
  - This is only suitable for traditional .Net applications, where you would put all these
  credentials into `web.config`/`app.config` file and use Transformation to set the value when
  running on different environment.
- As a .Net (Core) developer, especially ASP.Net (Core) developer, this is confusing for me. It
  increases the complexity of the application in an unnecessary way. Why? Microsoft have already
  provided a standard for storing and loading hard-coded values. It is to use the `appsettings.json`
  file. Why do we have to make it more complex? Why do we have to invent another method for that
  purpose? The first time when I read the code, I saw all these classes along with other
  `appsettings.json` file, that confused me a lot.
- It is not easy to update the configuration values. To change a single value, you will need to
  update your code, recompile it and trigger a redeployment of the app, which can take a lot of
  time. A better way should be to store them in environment variables, change the environment
  variables and then reload your app to run with the new values. Read more about `Config` in
  `12factor` app [here](https://12factor.net/config).

{% highlight csharp %}
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
{% endhighlight %}

The above code piece tries to protect the developer from accidentally sending a test email to real
customers in Local or Staging environment by replacing the input email with a fake/internal one.

However...

- As I mentioned before, what if a new developer unintentionally start the app with Prod
  configuration? Who knows what will happen?
- It is limited to just 3 environments: **Local**, **Staging** and **Prod**. What if you want to add
  one more environment, perhaps another **Staging** environment for a new team? You will have scan
  through your code base, find all the places using `swtich`/`case` and update them. No, it's not a
  good idea at all. We had this problem before when the Vietnam team started working on an existing
  project of the US team and we had to set up a new environment for the Vietnam dev team.
- The business logic is mixed with the Configuration. In an ideal case, **Configuration** should be
  strictly separated from the code logic. The code should have no realization of the environment it
  is running on so it is easy to scale the app, to add more instances running in different
  architecture (**Docker**, **Kubernetes** or just an instance added on a temporary server to help
  cope with the increase of workload)

> A litmus test for whether an app has all config correctly factored out of the code is whether the
> codebase could be made open source at any moment, without compromising any credentials.
> [The Twelve Factor App](https://12factor.net/config)

# Let's fix it

So, what are the requirements?

- Configuration should be separated from code logic. The code should just focus on business
logic and should not contain any of the logic to check for specific environment (like all the
switch/case we saw above).
- We should be able to override any specific value seamlessly. It should allow us to enable or
  disable any feature easily by turning on an off some environment variables rather than scanning
  through all places in the code to change manually.
- It should be easy to switch to another environment.
- Of course, environment types and credentials should not be hard-coded in code.

![Flow](/files/2020-04-07-stop-doing-this-you-have-a-better-way-to-write-config-in-net-core/flow.png)

1. All the setting values are loaded from different locations, through a set override order that we
   defined. This is the normal way that you would do with
   [ASP.Net Core](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration/?view=aspnetcore-3.1).
   The final result would be an instance of `IConfiguration`, which is a key-value dictionary of all
   the raw setting values. This allows us to switch the settings very easily by changing the input
   files or just some specific environment variables.
2. For simplicity in usage, the `IConfiguration` instance will be injected into another layer of
   custom Config object, where you can build, validate and transform all the raw string values to
   the correct format that you want.
3. The App components then consume the configuration simply by resolving the `AppConfig` instance
   from DI Container.

## 1. Load Configuration values

Microsoft has already documented and explained all the steps here build and register
`IConfiguration` with DI Container here
[Configuration in ASP.NET Core](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration/?view=aspnetcore-3.1).
Basically, what you need to do is just specify the load order when creating `IHostBuilder` instance.

{% highlight csharp %}
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
{% endhighlight %}

If you are writing your own Console application (or other types of application which are not ASP.Net
Core), simply install these extensions from Microsoft on Nuget

* Microsoft.Extensions.Configuration
* Microsoft.Extensions.Configuration.Json
* Microsoft.Extensions.Configuration.FileExtensions
* Microsoft.Extensions.Configuration.EnvironmentVariables

Here is a simple example showing how to register it with **Autofac Module**

{% highlight csharp %}
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
{% endhighlight %}

## 2. Transform

{% highlight csharp %}
public class AppConfig
{
    public string LogLevel { get; set; }
    public string MailgunApiKey { get; set; }
    public bool SendRealEmail { get; set; }
    public string DefaultToEmail { get; set; }

    public AppConfig(IConfiguration config)
    {
        LogLevel = config.GetString("LOG_LEVEL", defaultValue: "Info");
        MailgunApiKey = config.GetRequiredString("MAILGUN_API_KEY");
        SendRealEmail = config.GetBool("SEND_REAL_EMAIL", defaultValue: false);
        DefaultToEmail = config.GetString(
            "DEFAULT_TO_EMAIL", defaultValue: "test@test.test"
        );
    }
}
{% endhighlight %}

{% highlight csharp %}
protected override void Load(ContainerBuilder builder)
{
    // register directly
    builder.RegisterType<AppConfig>();

    // or if you want an interface
    builder.RegisterType<AppConfig>().As<IAppConfig>();
}
{% endhighlight %}

## 3. Consume

{% highlight csharp %}
public class SendEmail
{
    private readonly AppConfig _config;
    public SendEmail(AppConfig config)
    {
        _config = config;
    }

    public async Task Execute(string email)
    {
        // whether to send real email?
        var sendRealEmail = _config.SendRealEmail;
        if (!sendRealEmail) email = _config.DefaultToEmail;

        //
        await mailgunClient.Send(email);
    }
}
{% endhighlight %}
