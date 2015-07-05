---
author: Sarunas Valaskevicius
title: CQRS/ES in Scala: an overview
toc: yes
---

# Where are we now?

[Last time](/posts/cqrs-es-in-haskell/) we've looked at implementing a CQRS/ES application in Haskell - a purely functional programming language. This gave us some insights of how we can compose the elements together. Today we're going to look at some existing CQRS/ES implementations in Scala.


# CQRS with Akka actors and functional domain models (01/2011)

This is one of the first examples of CQRS in scala. And it also is written in a mostly functional style. One particularly interesting thing to note is that the `TradeLifecycle` is written as a Finite State Machine actor, which I think is a nice way to encode that an aggregate can be in a few different phases through its lifetime.

Both the aggregate and the projection in this example are modelled as Akka actors. One of the drawbacks of this example is that it is not using commands - the messages that are sent to the aggregate (`TradeLifecycle`) are events and there is no domain logic validation.

The event log that is provided by the example is another Akka actor. It is used as a write-only backend to log the events asynchronously. I would argue that such functionality does not guarantee the successful writes, however, we are discussing a rather minimal example so this can be expected.

Links to the source of the example: [blog](http://debasishg.blogspot.co.uk/2011/01/cqrs-with-akka-actors-and-functional.html), and [github](https://github.com/debasishg/cqrs-akka).

# Akka DDDD template (03/2015)

A Distributed Domain Driven Design example which is aiming to provide a fully scalable CQRS/ES solution as a template for new applications. Both commands and projections (queries) are Akka cluster actors, sharded across the available nodes. The HTTP endpoint is a simple Spray instance and it only proxies the messages to an appropriate cluster.

The write model - `BidProcessor` is modeled as a persistent actor, and also acts as a state machine, switching to different phases based on the processed commands. When a command has been processed, it notifies the read side that it should update the view.

Because each command processor is a persistent actor on a cluster node, we get a similar to pessimistic concurrency model for aggregates - commands are queued before the aggregate rules are applied. However instead of using locks the implementation mechanism here is based on communication channel to the actor thread. Potentially, this is a competing solution to optimistic concurrency control we saw in the Haskell application. Instead of retrying the command on a transaction failure, this approach sequences the incoming commands for a specific aggregate.

The view side, or in other words the projection, receives the events from the write side via the Akka's Persistence module's views. And updates its data representation according to the persisted events. Currently, the write model in the example notifies the projection to update synchronously, however Akka supports asynchronous updates as well.

The source code of the described example is available on [github](https://github.com/boldradius/akka-dddd-template#master).

# Reactive DDD with Akka (03/2015)

- [blog](http://pkaczor.blogspot.co.uk/2014/04/reactive-ddd-with-akka.html)
- [github](https://github.com/pawelkaczor/ddd-leaven-akka-v2)

# CQRS template (04/2015)

- [github](https://github.com/jgordijn/cqrs_template) -

# Final thoughts
- They all use Akka to model an aggregate.
- Use of FSM to model aggregates' lifetime phases
- this results in pessimistic concurrency model
