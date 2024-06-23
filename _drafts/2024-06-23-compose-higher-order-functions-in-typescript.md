---
layout: post
title: "Compose Higher Order Functions in Typescript"
description: ""
categories: [javascript]
tags: []
---

# Initial problem

It's the classic logging issue again. In the Warehouse Management system that I'm working on, we
usually need to add this logging pattern

```typescript
type Result = { outcome: 'SUCCESS'|'NO_AVAILABLE_STATION' };
const sendToteToPackingStation = (warehouseId: string, toteId: string): Promise<Result> => {
  logger.info('Sending tote to packing station', { warehouseId, toteId });

  const result = await someLogic(...);

  logger.info('Send tote result', { result });
  return result;
};
```

The purpose is simple. It's what you have to do for production debugging. You will need to log out
some unique ids so you have a place to query. From that point, you will start tracing the related
entries using a correlation id that your system provides.

From time to time, when the system scales up, there will be areas that slow down the system.
We then added more logic to the system, for example, execution time logging to help build some
visualization dashboards to identify the slowness.

```typescript
const sendToteToPackingStation = (warehouseId: string, toteId: string): Promise<Result> => {
  const startTime = performance.now();

  logger.info('Sending tote to packing station', { warehouseId, toteId });
  const result = await someLogic(...);
  logger.info('Send tote result', { result });

  logger.info('Execution time', { durationMs: elapsedTimeMs(startTime) });
  return result;
};
```

Of course, when we have to repeat this a lot, we started thinking about making a higher order
function for it to reuse everywhere is the system

# First implementation...

Let's begin with some type definition. Here are the generic types how a HOF looks like. It's a
function that receives a function and return another function with the same signature with the
input function

```typescript
export type WrappedFunction<
  FunctionArgs extends ReadonlyArray<unknown>,
  FunctionReturn
> = (...args: FunctionArgs) => FunctionReturn;

export type HigherOrderFunction = <
  FunctionArgs extends ReadonlyArray<unknown>,
  FunctionReturn
>(
  func: WrappedFunction<FunctionArgs, FunctionReturn>
) => WrappedFunction<FunctionArgs, FunctionReturn>;
```

<!-- more -->

And here is the first basic implementation

```typescript
export const withLogging: HigherOrderFunction =
  (wrappedFunction) =>
  (...args) => {
    const functionName = wrappedFunction.name || wrappedFunction.constructor.name;
    logger.info(`Running function ${functionName}`, { args });
    const startTime = performance.now();

    const result = wrappedFunction(...args);
    logger.info(`Finished ${functionName}`, {
      result,
      durationMs: elapsedTimeMs(startTime)      
    });

    return result;
  };

const sendToteToPackingStationFn = (warehouseId: string, toteId: string): Promise<Result> => {...}
export const sendToteToPackingStation = withLogging(sendToteToPackingStationFn);
```

# ...but it introduced other problems

- There are some cases that we want to log only the input and the execution time because the output
object is too large and can slow down the system. We then introduced another HOF
`withParamsLogging`. There are many overlapping HOFs like that.
- For function output logging, depending on the case, we have different ways of returning data.
Sometimes, it's a normal object with a `status`/`outcome` prop. There are places that we have to
return an object from [neverthrow](https://www.npmjs.com/package/neverthrow). It's hard to make
an HOF for multiple purposes. The HOF finally ended up being too complex and contains a lot of 
`if/else` to make sure we log the right way.
- We cannot use just part of the logic of the HOF, for example, only log execution time of the input
function.

# Composable Smaller HOFs

Inspired by [Redux compose](https://redux.js.org/api/compose) function, it didn't take much time
to resolve all the above problem with a composable approach. Instead of making big and generic HOFs,
I decided to split them into smaller HOFs, each HOF is responsible for 1 and only 1 purpose, for
example

- `withInputLogging` and `withOutputLogging` to log the input and output of a function
- `withNeverthrowOutputLogging` for logic specific to logging **neverthrow** output object
- `withExecutionTimeLogging`
- `withPubsubLogging` - automatically send the data to pubsub
- `withUnhandledErrorLogging` - beautify the unhandled error and rethrow it
- ...

Implement a simple compose function like this

```typescript
export const compose =
  (...hofs: Array<HigherOrderFunction>): HigherOrderFunction =>
  (wrappedFunction) =>
  (...args) => {
    if (!hofs.length) {
      return wrappedFunction(...args);
    }

    const [head, ...tail] = hofs;
    const finalFunc = tail.reduce(
      (res, hof) => hof(res),
      head(wrappedFunction)
    );

    return finalFunc(...args);
  };
```

And then you can decide which HOFs you want to use in each individual case

```typescript
export const sendToteToPackingStation = compose(
  withInputLogging,
  withOutputLogging,
  withExecutionTimeLogging,
)(sendToteToPackingStationFn);
```

# Make it work with Promise

You may notice that `withExecutionTimeLogging` or `withOutputLogging` don't really play nicely with
Promise. To fix it, add a `finally` block if the return data is a Promise.

```typescript
const result = wrappedFunction(...args);

if (isPromise(result)) {
  result.finally(() => logFinishTime());
} else {
  logFinishTime();
}
```

# Include extra information

You may find yourself need to include some extra information to the HOFs, for example, to log a key
that you can use to correlation the business logic. In that case, simply turn those that HOF to a
function that return the HOF (yes, it's a function returning a function returning a function 😅).

```typescript
export const withInputLogging =
  (opts: { action?: string } = {}): HigherOrderFunction =>
  (wrappedFunction) =>
  (...args) => {
    logger.info('Start hof', { action, args })
    //...
  };

// or to specify the pubsub topic to publish to?
export const withPubsubLogging =
  (opts: { topic: string }): HigherOrderFunction =>
  (wrappedFunction) =>
  (...args) => {
    client.publish(topic, { args });
    //...
  };
```