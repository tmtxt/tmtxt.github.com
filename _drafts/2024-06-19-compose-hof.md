---
layout: post
title: "Compose Higher Order Functions in Typescript"
description: ""
categories: [javascript]
tags: []
---

# Initial problem

```typescript
type Result = { outcome: 'SUCCESS'|'NO_AVAILABLE_STATION' };
const sendToteToPackingStation = (warehouseId: string, toteId: string): Promise<Result> => {
  logger.info('Sending tote to packing station', {warehouseId, toteId});
  const result = await someLogic(...);
  logger.info('Send tote result', { result });
  return result;
};
```

and then want to add execution time tracking
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

# First implementation

define the type first

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

and then the basic implementation

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

other problems arise

# Smaller Higher Order Functions

```typescript
export const withParamsLogging: HigherOrderFunction =
  (wrappedFunction) =>
  (...args) => {
    const functionName = wrappedFunction.name || wrappedFunction.constructor.name;
    logger.info(`Running function ${functionName}`, { params: args });
    return wrappedFunction(...args);
  };

export const withOutputLogging: HigherOrderFunction =
  (wrappedFunction) =>
  (...args) => {
    const functionName = wrappedFunction.name || wrappedFunction.constructor.name;
    const result = wrappedFunction(...args);
    logger.info(`Finished ${functionName}`, { result });
    return result;
  };
```

# Make it work with Promise

```typescript
// WIP
export const withExecutionTimeLogging: HigherOrderFunction =
  (wrappedFunction) =>
  (...args) => {
    const startTime = performance.now();
    const result = wrappedFunction(...args);

    if (isPromise(result)) {
      result.finally(() => logFinishTime());
    } else {
      logFinishTime();
    }

    return result;
  };

```