---
author: Sarunas Valaskevicius
title: Eventflow: CQRS/ES in Haskell
toc: yes
---


What is it all about?
=====================

[CQRS](http://martinfowler.com/bliki/CQRS.html) is not a new architectural pattern. And it is used more and more in complex applications, that require a highly scalable infrastructure.

The main principle of CQRS is based on [CQS](http://martinfowler.com/bliki/CommandQuerySeparation.html) - the Command Query Separation principle. It states that a method in object oriented paradigm must by either a command or a query. Commands should only concern themselves about changing data. In contrast, queries must not change the data but only return it to the client.

CQRS takes this principle a step up. It [describes](http://codebetter.com/gregyoung/2010/02/16/cqrs-task-based-uis-event-sourcing-agh/)  that the command methods should be segregated from the query methods. Instead of having one model for the data there are now two - the write model for commands, and the read model for queries. In effect, it is now possible maintain and optimise the write side separately from the read side.

<!--more-->

[Event Sourcing](http://martinfowler.com/eaaDev/EventSourcing.html) is an architectural style where the application is required to _persist all changes as new Event records_. Events are the primary source of truth when restoring the state. In comparison to only persisting the flat data, this way of storing the changes provides a few benefits:

  - Even if there was an error and the final state is incorrect, it can be recomputed completely. This is because the full history of data changes is preserved.
  - One could also tell what was the application state at any given point in time.
  - Provide retrospective statistics of the application's usage.
  - Frequently, there is a lot of time spent in database operations - calculating which rows are changing, locking them, and managing transactions. Event sourcing minimises the requirements for such complexity - the only operation used is append to the data store.

Both architectural styles solve of different problems and can be [combined](http://codebetter.com/gregyoung/2010/02/13/cqrs-and-event-sourcing/) to get the benefits of both and more. CQRS/ES application usually has enough flexibility to express the needs of different businesses. Yet, it is also of a rather restricted structure - such restrictions define the shape of the infrastructural components allowing for more reusability across applications. Using off-the-shelf products is one of the major ways to avoid much of the [accidental complexity](http://worrydream.com/refs/Brooks-NoSilverBullet.pdf) and to reduce the development cost.

In this article I will show an example of how to implement a CQRS/ES web application in Haskell. I will be using a simple domain of increment counters - one for which CQRS/ES pattern is possibly an overhead, but the mechanics for a more complex domains are similar so it is a good starting point to describe them.

As the article is written in [Literate Haskell](http://www.literateprogramming.com/), I will start with the language extensions to be used by the example:

```haskell
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
```

The imported extensions will help to serialise the data to JSON automatically and will also overload string literals so that they will not need to be casted to the `Text` type explicitly.

The application imports are separated to three sections: web framework, JSON handling library and generic Haskell libraries:

```haskell
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Method as Http
import qualified Network.HTTP.Types.Status as Http
import           Network.Wai               as W
import           Network.Wai.Handler.Warp  (run)

import           Data.Aeson
import           GHC.Generics

import           Control.Applicative       ((<$>))
import           Control.Concurrent
import           Control.Monad             (guard, when)
import qualified Data.Traversable          as Traversable
import           Data.Maybe                (fromMaybe, listToMaybe, isNothing)
import qualified Data.ByteString.Lazy      as B
import qualified Data.Text                 as T
import qualified Data.Map.Strict           as Map
```

Domain
======

Events
------

When growing an event sourced application, we automate the business processes based on the events happening in them. In a process modelled as a state machine, an event would denote a transition, that has already taken place and it carries all the data needed to describe it completely. In terms of business processes, an event is an _irrevocable record that an action has happened_. As such, it is good to [model events](http://ziobrando.blogspot.co.uk/2013/11/introducing-event-storming.html) based on the business the application is automating - the changes in business processes can be projected to the application with a minimal level of the required translation between the business language and the application structure.

As parts of the domain have to be consistent at all times, we'll draw boundaries around those parts and call them [aggregates](http://geekswithblogs.net/Optikal/archive/2013/04/07/152643.aspx). Commands handlers, when validating if a particular command can be executed, will receive all events of the same aggregate and will have to produce a set of events, that are consistent within the aggregate. We'll discuss about commands and their consistency more in depth later. For now, it's only enough to decide what is going to be the type of an identifier used for aggregates. I have chosen a `String` for this example as the counters in the domain are named thus it is a natural key for the aggregate, as long as its not empty:

```haskell
newtype AggregateId = AggregateId String deriving (Eq, Show, Ord, Generic)

aggregateIdFromString :: String -> Maybe AggregateId
aggregateIdFromString "" = Nothing
aggregateIdFromString s = Just $ AggregateId s
```

As the system I am modelling in this example is very simple, I have only defined two domain events:

```haskell
data Event = CounterCreated AggregateId
           | Incremented deriving (Eq, Show, Generic)
```

We can either create a new counter, or increment it if it exists.

Commands
--------

Before we talk about specific commands, lets see how should they look from outside:

```haskell
type CommandHandler = [Event] -> RouteParameters -> Maybe [Event]
```

A command handler takes a history of events, parameters for the current action, and results in a list of new events it has created. This is the main place where the business logic is enforced - and thus the commands can fail. To denote a failed command we'll ask it to return `Nothing` - for this we wrap the return type in the `Maybe` type.

We have only defined two commands for our domain - to create a new counter and increment an existing one.

```haskell
createCounterHandler :: CommandHandler
createCounterHandler events args = do
  aggregateId <- aggregateIdFromString =<< Map.lookup "id" args
  validateThatIsNew aggregateId events
  return [CounterCreated aggregateId]
    where
      validateThatIsNew aggregateId = guard . notElem (CounterCreated aggregateId)
```

First, `createCounterHandler` takes an `id` of the counter to create and converts it to the internal representation. Please note that as this function is running in a `Maybe` monad - the map lookup can return `Nothing` and the counter creation will fail. In a similar fashion, we check if the same identifier has not been used before to create a counter, and guard against that case as well.
To those wondering why is the second argument missing in the `validateThatIsNew` function definition, whilst it exist when we call it, please see the [pointfree](https://wiki.haskell.org/Pointfree) programming style. The pointfree style, when applied with care, results in simple, pleasant for the eyes code and is generally the preferred style to express functions.

```haskell
incHandler :: CommandHandler
incHandler events args = do
  aggregateId <- aggregateIdFromString =<< Map.lookup "id" args
  validateThatIsCreated aggregateId events
  return [Incremented]
    where
      validateThatIsCreated aggregateId = guard . elem (CounterCreated aggregateId)
```

The increment command handler starts similarly as the counter creation - however it validates that the counter exists - it is not possible to increment a non existing counter.


Projections
-----------

While the main purpose of commands is to enforce the consistency of the business rules in applications, projections solve a different problem. The purpose of projections is to optimise the data for the read side. Queries will use the data flattened by projections to retrieve the results for the clients quickly. As such, when commands are creating new events in the system, the projections race to catch up with them and modify their projected data representations. There can be as many projections as there are use cases in the system - and as the number can be high, or involve complex computations, they are run in parallel and do not block commands. Not blocking the commands means higher throughput on the write side and easier scalability as you can run projections on any number of nodes without needing to lock the write side. The drawback of this is that they are [eventually consistent](http://www.allthingsdistributed.com/2007/12/eventually_consistent.html) - that is, the data in the projections will be stale or, in other words, slightly out of date.

I'll start by defining a generic data type for a projection:

```haskell
data Projection a = Projection {
      initProjection :: a,
      runProjection  :: a -> Event -> AggregateId -> a
  }
```

A projection will maintain its own data structure of type `a`, where it flattens the incoming system events. `initProjection` will know how to initialise the custom data structure for the particular use case. Once we know the initial state, we will use `runProjection` that accepts the state before an event, a system event, an aggregate id and will produce an amended data structure with the effects of the given event applied.

Let's create our first projection. As we want to list all created counters, our first projection will maintain a flattened list of all counter identifiers:

```haskell
type AllCountersProjectionState = [AggregateId]
```

The projection will be initialised with an empty list, that will be appended by any new created counters:

```haskell
allCountersProjection :: Projection AllCountersProjectionState
allCountersProjection = Projection {
  initProjection = [],
  runProjection = runner
} where
  runner counters (CounterCreated counterId) _ = counters ++ [counterId]
  runner counters _ _ = counters
```

Simple! Our next projection is just a tiny bit more complicated - it will maintain a map of counter identifiers to a number of times it was incremented:

```haskell
type CounterProjectionState = Map.Map AggregateId Int
```

When a counter is created, we initialise its count to zero. We will increment a counter's value by one for every `Incremented` event it receives:

```haskell
counterProjection :: Projection CounterProjectionState
counterProjection = Projection {
  initProjection = Map.empty,
  runProjection = runner
} where
 runner projectionData (CounterCreated counterId) _ = Map.insert counterId 0 projectionData
 runner projectionData (Incremented) aggregateId = Map.adjust (+ 1) aggregateId projectionData
```

Not a rocket science at all! In more complex systems the projections could push data to a relational/document/graph or any other database as well so that queries could later use them for their specialised indexes - all while the write side is not blocked and is continuously accepting new commands.

Queries
-------

The purpose of queries is to serve the data to the client. There can be a few types of queries as well - a single HTTP request, or a socket, where the data is being continuously pushed to the client. And there can be different formats that clients accept. For the sake of simplicity I will only use HTTP queries, that send the result as JSON. Also, as queries are reading their data from the projections, the type we will define for queries is a single function, that accepts the projection data persistence channel, and results in a HTTP endpoint:

```haskell
type Query a = ProjectionStatePersistence a -> RouteAction
```

As we now have the query type, let's define a simple query to retrieve all created counters.

```haskell
allCountersQuery :: Query AllCountersProjectionState
allCountersQuery persistence _ _ = do
  counterStorage <- readMVar persistence
  return . sendPlainJson . getProjectionData $ counterStorage
```

To serve the requested data, we read from the projection's persistence, extract its data, and send it back as JSON.

A slightly more complex query - to retrieve the counter value - will first try to find the requested counter in the projection's data, extract the counter value from the `id -> counter` map, and send it as JSON as well. To achieve the task we will first need to extract the requested counter identifier from the query parameters, and convert it to a valid `AggregateId` - both operations can fail (either the counter identifier is missing in the parameters, or it is not passed in a valid format) - to encode this, our `aggregateId` will use the `Maybe` monad we have seen before.

```haskell
counterQuery :: Query CounterProjectionState
counterQuery persistence params _ = do
  storage <- readMVar persistence
  case aggregateId of
    Nothing -> return $ send404 "Aggregate not found (wrong id).\n"
    Just aggId ->
        case Map.lookup aggId $ getProjectionData storage of
            Nothing -> return $ send404 "Aggregate not found.\n"
            Just counter -> return $ sendPlainJson counter
  where
    aggregateId :: Maybe AggregateId
    aggregateId = aggregateIdFromString =<< Map.lookup "id" params
    send404 = responseLBS Http.status404 [("Content-Type", "text/plain")]
```

Once we know that the `aggregateId` is not `Nothing`, we try to find the requested counter and returns its value as JSON.

Of course, in more complex applications, it would be beneficial to separate the knowledge of HTTP and the domain. This would allow us to handle different output formats, or even different transports via composition and result in a more flexible solution.

Infrastructure
==============

Persistence
-----------

Persisting the data in an application is one of the factors that makes it useful. For simplicity, we will be storing the serialised event data to memory, without actually persisting to disk - this aspect would of course change in a real life application but for now let's try to avoid the complexity.

Let's start with some types:

```haskell
type Version = Int
data VersionedEvents = VersionedEvents Version (Map.Map Version B.ByteString)
type AggregateEvents = Map.Map AggregateId VersionedEvents
type Persistence = MVar AggregateEvents
```

The events for aggregate are stored separately from each other - this way we are still able to manage the event storage on per aggregate/stream basis, following the structure of the [Event Store](https://geteventstore.com/). Also, the events for each aggregate have a version attached - each time a command is executed we'll store the events for the given version in `VersionedEvents` and will update the top level version to the latest value.

To initialise the persistence we simply need to construct a new variable in memory:

```haskell
initCounterPersistence :: IO Persistence
initCounterPersistence = newMVar Map.empty
```

To help us to use the new `VersionedEvents` type, let's define a few functions for it:

```haskell
initVersionedEvents :: VersionedEvents
initVersionedEvents = VersionedEvents 0 Map.empty
>
getEventsVersion :: VersionedEvents -> Version
getEventsVersion (VersionedEvents v _) = v
>
getEventsMap :: VersionedEvents -> Map.Map Version [Event]
getEventsMap (VersionedEvents _ m) = Map.map (fromMaybe [] . decode) m
>
getEventsMapFromVersion :: VersionedEvents -> Version -> Map.Map Version [Event]
getEventsMapFromVersion (VersionedEvents _ m) v = Map.map (fromMaybe [] . decode) newEvents
  where
    (_, newEvents) = Map.split v m
>
addNewEventsVersion :: Version -> [Event] -> VersionedEvents -> VersionedEvents
addNewEventsVersion v' e' (VersionedEvents _ e) = VersionedEvents v' $ Map.insert v' (encode e') e
```

We initialise the versioned events to start from zero. Also, we create a function to retrieve the latest event version, and both full, and partial version to event maps. In case there was a failure decoding the list of events from the serialised data we will return an empty list for them.

Please note, that in `addNewEventsVersion` and `getEventsMap*` functions we also manage the data serialisation - events are being converted to, and from JSON format using `encode` and `decode` functions from the Aeson library. To support such data translation we will need to declare that the types are instances of from JSON handling typeclasses:

```haskell
instance ToJSON AggregateId
instance FromJSON AggregateId
instance FromJSON Event
instance ToJSON Event
```

Because the `AggregateId` and `Event` types are deriving the `Generic` typeclass, we can simply use the default JSON conversion functions for them.

Command handling
----------------

The commands we have seen in previous paragraph take a list of past events, the input from the client and produce a list of new events. Simple! Or is there more to it?

To implement such commands handlers we will need to define behaviour of some common, lower level command handling responsibilities: the past events need to be read from the persistence layer, and the results written back to it. If there are several commands invoked at the same time we will also need to deal with concurrency issues. These are exactly the topics I will talk about in this paragraph.

The possible execution results of a particular command handler are as follows:

```haskell
data ExecutionState = CommandFailure | CommandSuccess | TransactionFailure deriving (Eq, Show)
```

The command fails when it fails to validate the business rules it encodes, otherwise it succeeds. However, if a command is successful, it can still result in a failure. Let's say we have two commands, running at the same time. When the first command succeeds, but the second command is executed and persisted just before the results of the first command could be stored, we need to check the validation rules of the first command again, and the current results of it are rendered invalid. Such state is called a failed transaction. One way to deal with transaction failures is [optimistic concurrency control](http://c2.com/cgi/wiki?OptimisticLocking) - we will retry the command up to a specified amount of times, passing the new events to it.

```haskell
commandRetryCount :: Int
commandRetryCount = 10
```

This will most likely solve the problem, especially if the failures are rare, however it does not guarantee that the user will not see a failure given enough concurrent commands that are affecting the same aggregate.

Another responsibility of handling a command is that when a command has been executed, and the results successfully stored, it would need to notify the event listeners about the new changes. To allow such functionality we'll use non-blocking channel with a defined message type:

```haskell
data NewEventMessage = NewEventMessage AggregateId Version deriving (Show, Eq)
```

We are now ready to look at running the command handlers:

```haskell
commandRunner :: CommandHandler -> Persistence -> Chan NewEventMessage -> RouteAction
commandRunner handler persistence eventChan params _ = do
  ret <- retryTransactionFailures commandRetryCount execute
  case ret of
    CommandFailure -> return renderCommandFailure
    CommandSuccess -> return renderSuccess
    TransactionFailure -> return renderTransactionFailure
  where
```

... running a command handler starts with stating that we will retry the `execute` function up to `commandRetryCount` times and will route the result to an appropriate rendering function. When executing, we will take the `id` parameter from the request, convert it to the `AggregateId` and continue to `executeOnAggregate`.

```haskell
    execute = do
        let aggregateId = aggregateIdFromString =<< Map.lookup "id" params
        case aggregateId of
            Just aggId -> executeOnAggregate aggId
            Nothing -> return CommandFailure
```

`executeOnAggregate` loads the event history for a particular aggregate, applies the command handler logic, and stores the new events if the command has been successful:

```haskell
    executeOnAggregate aggId = do
        versionedEvents <- loadEventStorage aggId
        case handler (eventList versionedEvents) params of
            Just events' -> storeNewEvents aggId versionedEvents events' (notifyListeners aggId)
            Nothing -> return CommandFailure
    eventList = concat . Map.elems . getEventsMap
```

As we are currently using the in-memory data store, loading the events from their persistence variable is trivial:

```haskell
    loadEventStorage aggId = do
        storage <- readMVar persistence
        return $ eventsForAggregate aggId storage
    eventsForAggregate = Map.findWithDefault initVersionedEvents
```

However, when we are storing the events, we are also incrementing the event version, so that we can check later if there were no version conflicts from other commands:

```haskell
    storeNewEvents aggId versionedEvents events' onSuccess = do
        let version' = getEventsVersion versionedEvents + 1
            versionedEvents' = addNewEventsVersion version' events' versionedEvents
        result <- modifyMVar persistence $ verifyAddToAggregate aggId version' versionedEvents'
        when (result == CommandSuccess) $ onSuccess version'
        return result
```

`verifyAddToAggregate` is the exact place where the optimistic locking is implemented - it is executed atomically within `modifyMVar` environment, so it is guaranteed, that no parallel check/event storing will be running at the same time. If the latest event version for the current aggregate is less than the currently added one, no one else has modified the events stream after our initial read and it is safe to update the data store with the new data. If the events version in the data store is same is greater than the version that is being added - there is a transaction failure and we do not modify the existing events:

```haskell
    verifyAddToAggregate :: AggregateId -> Version -> VersionedEvents -> AggregateEvents -> IO (AggregateEvents, ExecutionState)
    verifyAddToAggregate aId version' events' storage = do
      let canChange = (< version') . getEventsVersion $ eventsForAggregate aId storage
      return $ if canChange then (Map.insert aId events' storage, CommandSuccess)
                            else (storage, TransactionFailure)
```

To notify the successful command listeners, we simply send a new event message - this function has been used as the `onSuccess` handler for the `storeNewEvents`:

```haskell
    notifyListeners aggId version = writeChan eventChan $ NewEventMessage aggId version
```

While retrying a command could be written in pattern matching style, we will defensively use guards syntax in the next function in case the parameter `c` is negative. The function simply repeats the given command if it results in a `TransactionFailure` up to `c` times:

```haskell
    retryTransactionFailures c command
      | c > 0 = do
          ret <- command
          if ret == TransactionFailure
            then retryTransactionFailures (c - 1) command
            else return ret
      | otherwise  = return TransactionFailure
```

The HTTP responses for the different command results:

```haskell
    renderCommandFailure = responseLBS Http.status403 [("Content-Type", "text/plain")] "Command validation failed.\n"
    renderSuccess = responseLBS Http.status200 [("Content-Type", "text/plain")] "Applied.\n"
    renderTransactionFailure = responseLBS Http.status409 [("Content-Type", "text/plain")] "Transaction validation failed. Please retry.\n"
```


Running projections
-------------------

Projections track the event streams that are produced by commands, and update their internal _projected_ state according to the use case. To support such functionality we start by defining suitable data types for the projection structure:

```haskell
type EventStreamPositions = Map.Map AggregateId Version
data ProjectionRunnerInfo a = ProjectionRunnerInfo EventStreamPositions a
type ProjectionStatePersistence a = MVar (ProjectionRunnerInfo a)
>
initProjectionPersistence :: Projection a -> IO (ProjectionStatePersistence a)
initProjectionPersistence p = newMVar (ProjectionRunnerInfo Map.empty $ initProjection p)
```

Projections, as the systems events, will be persisted only to the in-memory variable. We also allow flexibility to define custom data type for the projection's data. What's more - each projection will have its own tracking state for the event streams - `EventStreamPositions` tells us what version for a particular stream the projection has been already caught up with.

To run a projection, we define a type for its process - which has access to the events persistence, projection data store, means to receive updates when there are new events in the system, and the projection itself:

```haskell
type ProjectionRunner a = Persistence -> ProjectionStatePersistence a -> Chan NewEventMessage -> Projection a -> IO ()
```

Once we have the projection's runner type, its rather easy to define the runner itself. It only has to do two things - run the projection for all past events until the current moment, and then start listening for new system events and update its state continuously:

```haskell
projectionRunner :: ProjectionRunner a
projectionRunner persistence projectionStatePersistence newEventsNotifier projection = do
  catchUpProjection
  listenForNewEvents
  where
```

Catching the projection up with the past events needs to end up updating the projection's state having folded the projection's update function through all events that have happened in the system.

```haskell
    catchUpProjection = do
      storage <- readMVar persistence
      modifyPersistence (\initialState -> return $ Map.foldlWithKey runProjectionForAggregate initialState storage)
```

Because we know that the structure of the `storage` variable is a map of `aggregateId` to its versioned events, we can fold through the aggregates first passing all aggregate events to the projection runner. Please note, that such fold does not preserve the order of system events that happened in separate aggregates, and may cause troubles - if there were different expectations by the projection's function. However, having in mind that an aggregate _is the consistency boundary_ such behaviour does not violate CQRS/ES principles.

Once the projection is up to date with the state of all aggregates, it switches to the 'listening' mode. The `readChan newEventsNotifier` will block until there is a message sent to the projection runner and once it is received - the particular aggregate will be updated.

```haskell
    listenForNewEvents = do
      (NewEventMessage aggregateId _) <- readChan newEventsNotifier
      updateForAggregate aggregateId
      listenForNewEvents
```

Updating a single aggregate will have to read the events happened to it and run them through the projection function:

```haskell
    updateForAggregate aggregateId = do
      storage <- readMVar persistence
      case Map.lookup aggregateId storage of
        Just events -> modifyPersistence (\state -> return $ runProjectionForAggregate state aggregateId events)
        Nothing -> return ()
```

Modifying projection's persistence is simply an alias for the full `MVar` modification function:

```haskell
    modifyPersistence = modifyMVar_ projectionStatePersistence
```

Finally, running a projection for an aggregate, that is used in both the catchup and listening phases, will fold the projection's function for all new events since the last update:

```haskell
    runProjectionForAggregate (ProjectionRunnerInfo streamPositions projectionData) aggregateId allEvents = ProjectionRunnerInfo streamPositions' projectionData'
        where
          v = Map.findWithDefault 0 aggregateId streamPositions
          newEvents = concat . Map.elems $ getEventsMapFromVersion allEvents v
          v' = getEventsVersion allEvents
          streamPositions' = Map.insert aggregateId v' streamPositions
          projectionData' = foldl (\pdata event -> runProjection projection pdata event aggregateId) projectionData newEvents
```

We take the current projection version (`v`) from the `streamPositions` for the aggregate, or start with zero. Then, we collect all events that have happened from the version `v` as a single list `newEvents`. Once we have the mentioned definitions, we can now express the fold through the new events, modifying the `projectionData` (`pdata` in the fold's lambda) which is part of our result value for the new `ProjectionRunnerInfo`.

The final bit related to the projections is to define how queries will be able to access its state - as queries usually don't need to know about the version of the projection they are querying:

```haskell
getProjectionData :: ProjectionRunnerInfo a -> a
getProjectionData (ProjectionRunnerInfo _ d) = d
```


Http api
--------

We now have all major elements for CQRS/ES application defined, but to complete the picture, lets see how we can actually use them. For this, we need to wrap them in some kind of an interaction layer, where the user of the system can send commands to and receive the results. As an example, I have used a simple HTTP server, listening on `http://localhost:3000/`.

As before, lets start with a few types we are going to use:

```haskell
type RequestPath = [T.Text]
type RouteMatcherConfig = [String]
type RouteParameter = String
type RouteParameterValue = String
type RouteParameters = Map.Map RouteParameter RouteParameterValue
type RouteMatcher = Http.Method -> RequestPath -> Maybe (Request -> IO W.Response)
type RouteAction = RouteParameters -> Request -> IO W.Response
```

The most notable types are the `RouteParameters`, which collect route info and pass to the commands/queries, and the `RouteAction`, which is a type of both the command runner and the system queries.

We want to use custom routing which can extract `RouteParameters` for the custom `RouteActions`. Let's start by defining the route matcher. It is configured with the route description and produces a route if the request metches the configured route format:

```haskell
matchRoute :: Http.Method -> RouteMatcherConfig -> RouteAction -> RouteMatcher
matchRoute method config action currentMethod requestPath
 | method == currentMethod = (action . Map.fromList) <$> matchConfiguredRoute config (map T.unpack requestPath) []
 | otherwise = Nothing
  where
```

For those new to Haskell, `<$>` means an `fmap` as an infix - if the `matchConfiguredRoute` returns `Nothing`, then the result of the whole function will be `Nothing`, otherwise - `Just x` will be converted to `Just (action (Map.fromList x))`.

Matching a route configuration is a recursive function, which also collects parameter values, if the route config starts with the `':'` character:

```haskell
    matchConfiguredRoute :: [String] -> [String] -> [(String, String)] -> Maybe [(String, String)]
    matchConfiguredRoute (c:cs) (rp:rps) params
      | listToMaybe c == Just ':' = matchConfiguredRoute cs rps (params++[(tail c, rp)])
      | c == rp = matchConfiguredRoute cs rps params
      | otherwise = Nothing
    matchConfiguredRoute [] [] params = Just params
    matchConfiguredRoute _ _ _ = Nothing
```

Routing a request will find a matching route by trying all matchers in sequence, until it finds one the does not result in `Nothing`:

```haskell
routeRequest :: [RouteMatcher] -> Request -> Maybe (IO W.Response)
routeRequest matchers request = fmap (\requestAction -> requestAction request) . matchingRoute $ matchers
  where
    currentMethod = requestMethod request
    currentPathInfo = pathInfo request
    applyMatcher matcher = matcher currentMethod currentPathInfo
    matchingRoute = listOfMaybeToMaybe . take 1 . dropWhile isNothing . map applyMatcher
    listOfMaybeToMaybe = fromMaybe Nothing . listToMaybe
```

A few functions for convenience of handling JSON output:

```haskell
sendJson :: ToJSON s => Http.Status -> [Http.Header] -> s -> Response
sendJson status headers dat = responseLBS status (headers++[jsonHeader]) $ encode dat
  where
    jsonHeader = ("Content-Type", "application/json")
>
sendPlainJson :: ToJSON s => s -> Response
sendPlainJson = sendJson Http.status200 []
```

Once we have the routing mechanism defined, we can express the main web application that uses it:

```haskell
application :: (t -> Maybe (IO Response)) -> t -> (Response -> IO b) -> IO b
application route request respond = do
  response <- Traversable.sequence $ route request
  case response of
    Just r -> respond r
    Nothing -> respond $ responseLBS Http.status404 [("Content-Type", "text/plain")] "Unsupported request."
```

Please note that the routing can fail resulting in `Nothing` - thus we use `Traversable.sequence` to handle the request in a functor-ish manner (whilst still staying in the IO monad).


The final bit of our example is to wire everything up and produce a working application. The `main` function is as follows:

```haskell
main :: IO ()
main = do
```

The event channel that will be used to notify projections when there are new events stored:

```haskell
  eventChan <- newChan
```

The main aggregate persistence:

```haskell
  counterPersistence <- initCounterPersistence
```

Let's start our first projection, as a forked process:

```haskell
  counterProjectionPersistence <- initProjectionPersistence counterProjection
  _ <- forkIO $ projectionRunner counterPersistence counterProjectionPersistence eventChan counterProjection
```

We'll run another projection, thus we need to duplicate the channel so it acts like a [broadcast](http://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Concurrent-Chan.html#v:dupChan) for new event messages:

```haskell
  eventChan2 <- dupChan eventChan
  allCountersProjectionPersistence <- initProjectionPersistence allCountersProjection
  _ <- forkIO $ projectionRunner counterPersistence allCountersProjectionPersistence eventChan2 allCountersProjection
```

Finally, let's start the HTTP listener:

```haskell
  let configuredRouteRequest = routeRequest (routeConfig eventChan counterPersistence counterProjectionPersistence allCountersProjectionPersistence)
  run 3000 (application configuredRouteRequest)
  where
    routeConfig eventChan counterPersistence counterProjectionPersistence allCountersProjectionPersistence =
      [ matchRoute Http.methodGet ["list"] (allCountersQuery allCountersProjectionPersistence)
      , matchRoute Http.methodGet [":id"] (counterQuery counterProjectionPersistence)
      , matchRoute Http.methodPost ["create", ":id"] (commandRunner createCounterHandler counterPersistence eventChan)
      , matchRoute Http.methodPost [":id"] (commandRunner incHandler counterPersistence eventChan)
      ]
```

That's it! the application can be now executed, and we can use HTTP to run the defined commands and queries:

````bash
$ curl -X POST http://localhost:3000/create/my-counter
Applied.

$ curl http://localhost:3000/my-counter
0

$ curl -X POST http://localhost:3000/my-counter
Applied.

$ curl http://localhost:3000/my-counter
1

$ curl -X POST http://localhost:3000/my-counter
Applied.

$ curl http://localhost:3000/my-counter
2
````

It is also worth to mention, that in this example, some of the HTTP API types have still leaked to the command runner and the queries. A real life application would add a translation layer that defines how the command/query IO is defined. This way it would be even more flexible and unrestricted by choice of the transport layer.

Summary and future work
=======================

We have built a very simple, yet fully functional named counters application - which we have split into two main parts: the _Domain_ - where the business logic resides, and the _Infrastructure_ - a subsystem allowing the Domain logic to be executed. When growing a software system, we start by the events, aggregates and the use cases in the domain, reusing much of the Infrastructure code from the previous projects. This allows us to cut the costs by a substantial factor, as the Infrastructure part is where a large part of costs usually hides - scaling processes, synchronising and persisting data, defining communication protocols and more. With Event Sourcing, we not only know the state of the system but how it got to it as well - debugging and error recovery was never easier!

Of course, this article has only shown a very simple CQRS/ES application - there are plenty of possilities to improve it, for example:

  - persist events to disk
  - command preprocessor - to authenticate, enrich and translate commands between different versions
  - allow projections to emit events
  - implement a more flexible subsciption model for projections to listen to specific aggregates, or aggregate types
  - when initialising projection, order events by their time, not aggregate
  - implement different projection backends for specialised access - using key value, relational, graph or other databases
  - different backends for queries - e.g. allowing push based notification (e.g. sockets) to clients
  - implement process for [sagas](http://blog.jonathanoliver.com/cqrs-sagas-with-event-sourcing-part-i-of-ii/)

Some of the items in the list have been inspired by the [Event Store](http://geteventstore.com/) and it could be used instead of implementing the features from scratch.

I hope the article has been useful to you - let me know! And if you have found an error to fix or an improvement to be made - I would appreciate your feedback!

