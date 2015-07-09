---
author: Sarunas Valaskevicius
title: CQRS/ES in Scala: an overview
toc: yes
---

# Where are we now?

[Last time](/posts/cqrs-es-in-haskell/) we've looked at implementing a CQRS/ES application in Haskell - a purely functional programming language. This gave us some insights of how we can compose the elements together. Today we're going to look at some existing CQRS/ES examples in Scala.


# CQRS with Akka actors and functional domain models (01/2011)

This is one of the first examples of CQRS in scala. And it also is written in a mostly functional style. One particularly interesting thing to note is that the `TradeLifecycle` is written as a Finite State Machine actor, which I think is a nice way to encode that an aggregate can be in a few different phases through its lifetime.

Both the aggregate and the projection in this example are modelled as Akka actors. One of the drawbacks of this example is that it is not using commands - the messages that are sent to the aggregate (`TradeLifecycle`) are events and there is no domain logic validation.

The event log that is provided by the example is another Akka actor. It is used as a write-only backend to log the events asynchronously. I would argue that such functionality does not guarantee the successful writes, however, we are discussing a rather minimal example so this can be expected.

Links to the source of the example: [blog](http://debasishg.blogspot.co.uk/2011/01/cqrs-with-akka-actors-and-functional.html), and [github](https://github.com/debasishg/cqrs-akka).

# Akka DDDD template (03/2015)

A Distributed Domain Driven Design example which is aiming to provide a fully scalable CQRS/ES solution as a template for new applications. Both commands and projections (queries) are Akka cluster actors, sharded across the available nodes. The HTTP endpoint is a simple Spray instance and it only proxies the messages to an appropriate cluster.

The write model - `BidProcessor` is modeled as a persistent actor, and also acts as a state machine, switching to different phases based on the processed commands. When a command has been processed, it notifies the read side that it should update the view.

Because each command processor is a persistent actor on a cluster node, we get a pessimistic concurrency model for aggregates - commands are queued before the aggregate rules are applied. Instead of explicitly using locks the implementation mechanism here is based on communication channel to the actor thread.

The view side, or in other words the projection, receives the events from the write side via the Akka's Persistence module's views. And updates its data representation according to the persisted events. Currently, the write model in the example notifies the projection to update synchronously, however Akka supports asynchronous updates as well.

The source code of the described example is available on [github](https://github.com/boldradius/akka-dddd-template#master).

# Reactive DDD with Akka (03/2015)

One immediately notable difference in this example is the modular structure - each bounded context is implemented in its own namespace - sales, shipping, invoicing. They are also split into separate parts: read-front, read-back, write-front, write-back. This is a nice approach to build modular systems, and allows to extend them easily without modifying the existing deployment.

This is the most complete example that I've seen so far. It not only includes the commands and projections, but also [sagas](http://cqrs.nu/Faq/sagas). There is an  `OrderSaga` in sales context, which either completes a reservation or cancels it depending on billing result. Unsurprisingly, it is implemented as an Akka actor.

The write side of this example is very similar to the previous one - using Akka's persistence module to store the events and repopulate an actor - command handler - from the past events.

However the read side here is different. Instead of using Akka's persistence module's views, which only allow to receive the updates from one aggregate, the example is using a custom Akka's serialisation support to send all events to the [Event Store](http://geteventstore.com/) and implements projections as provided by Event Store.

While initially the structure of the application looks rather complicated, having read it carefully it I think it is clean and would benefit large systems well. Having said that I'd look for more concise ways to represent the domain for each bounded context.

The blog posts can be found at: [blog post 1](http://pkaczor.blogspot.co.uk/2014/04/reactive-ddd-with-akka.html), [blog post 2](http://pkaczor.blogspot.co.uk/2014/04/reactive-ddd-with-akka-lesson-2.html) and [blog post 3](http://pkaczor.blogspot.co.uk/2014/06/reactive-ddd-with-akka-projections.html). Also there are github repositories for both the [akka-ddd](https://github.com/pawelkaczor/akka-ddd) library and the discussed example [ddd-leaven-akka-v2](https://github.com/pawelkaczor/ddd-leaven-akka-v2).


# Final thoughts

All examples that I've seen in Scala use Akka. This is not surprising, as actor model fits an aggregate's description well, and Akka provides a good implementation of it. The resulting systems benefit from Akka's provided routing of messages and the persistence module. It solves most of the infrastructural complexity. I am curious however to see how an Akka based CQRS system performs in a write-intensive domain - or what is the limit where an alternative implementation with e.g. an optimistic concurrency control would be a better choice?

And whilst Akka provides a good mechanism for the write side, it is less appropriate to write projections in. Many projections need to listen for events coming from a few aggregates, which is not provided by the default persistence module views. A custom message bus is needed to implement such functionality. And integration with other event systems that are running projections, such as the Event Store, works well too. Maybe Akka's streams module provides what is needed?

A few examples have used finite state machines to define the commands an aggregate can accept. This works well, especially in more complex aggregates, as it both provides a clear view of possible transitions in the system and also ensures its correctness. What I've missed and believe would add even more value is to declare the aggregate events in a more concise manner - a DSL describing the state transitions could work well.

