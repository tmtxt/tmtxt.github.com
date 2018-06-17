---
layout: post
title: "Feature Toggle and Feature Release"
description: ""
categories: [misc]
---

Feature Toggle is a very popular technique that enables you to test the feature on real production
environment before releasing it to your clients. It's also helpful when you want to enable the
feature for just some beta clients or just some clients who pay for the specific features. The
technique requires both backend and frontend work involved. In this post, I'm going to talk about
some solutions that I and the team at AR have applied as well as some other useful ways that we are
still discussing and may apply one day in the future.

# 1. Backend Data Organization

## Feature Flag table

Of course, the simplest solution is to create a specific table for storing the all the feature flags
in the database. The table may looks like this

```javascript
{
  featureName: <string>,
  released: <bool>,
  enabledList: <array>, // enabled clients list
  disabledList: <array> // disabled clients list
}
```

The above mentioned organisation may be suitable for the case your system has a lot of users. You can simply add some admin user to the `enabledList` and test the new feature on production before releasing it to your users.

## Inline User feature data

If your product is to serve business clients, you can also store the enabled feature directly to the client object itself. This can save you extra queries to the database to get the feature information. If that's the case, your Client object might look like this

```javascript
{
  clientId: <string>,
  enabledFeatured: <array>
}
```

## Unix Permission style

So far the above solutions all return a format with a string indicating the feature name and
probably a boolean for the feature status. After when your application grows to a bigger size, this
may create a huge problem if you have a lot of feature. The size to store, query and transfer all
those features configurations through network may be a big performance issue. Luckily, there is
another solution for that. In case you store the configuration option per each client, you can
describe it as a series of bits similar to the Unix file permission style `1` for enabled features
and `0` for disabled ones. For example

|Client|Feature1|Feature2|Feature3|Feature4|Result|
|---|---|---|
|Client1|1|0|0|1|9|
|Client2|1|0|0|0|8|
{: .table }

The final result stored in database is just one simple int number. It much better than storing and
array of enabled features. With this solution, one single 8-bit integer can represent up to 8
different feature configurations.

## Some Optimizations

Including one extra query to the database per request to check for the enabled feature may not be a
good idea. Usually the features don't change very frequently unless the admin decide to update or
release any new feature. It's better if you can store these information in some caching system like
Redis.

# 2. Frontend Feature toggle with React/Redux

> The reason why I mention `React` and `Redux` here is because the product that I'm working on
> currently is a web application built on top of `React` and `Redux`.

By applying Higher Order Component concept, it's very easy for us to dynamically enable/disable
specific feature on our web application. The Higher Order Component looks like this

```javascript
const injectFeature = featureName => WrappedComponent => {
  class InjectFeatureWrapperComponent extends Component {
    // load the feature configuration if not loaded yet
    componentDidMount() {
      if (!this.props.featuresLoaded) {
        store.dispatch(loadFeatures());
      }
    }

    render() {
      // inject the feature toggle props into the WrappedComponent
      const { featureEnabled } = this.props;
      const propName = `${featureName}Enabled`;
      const injectedProps = { [propName]: featureEnabled };
      return <WrappedComponent {...this.props} {...injectedProps} />;
    }
  }

  // select the feature configuration from the store
  const mapStateToProps = state => {
    const { featuresLoaded, featureEnabled } = selectFeatureFromStore(store, featureName);
    return { featuresLoaded, featureEnabled };
  };

  // connect the component to the store
  const HOC = connect(mapStateToProps)(InjectFeatureWrapperComponent);

  return HOC;
};
```

In any of your component, to check for a specific feature enabled or not, simply wrap it inside the
HOC above

```javascript
class MyComponent extends Component {
  render() {
    const { sendMailEnabled } = this.props;
    if (!sendMailEnabled) {
      return (
        <div>
          You are not allow to use this
        </div>
      );
    }

    return (
      <SendMail>
        Render Send Mail component here
      </SendMail>
    );
  }
}

export default injectFeature('sendMail')(MyComponent);
```

One cool thing about this is if you are the super admin who updates the feature configurations, you
will see your web application "react" to the changes and render the feature immediately without
having to reload the whole page.
