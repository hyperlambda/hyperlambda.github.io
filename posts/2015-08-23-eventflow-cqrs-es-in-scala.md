---
author: Sarunas Valaskevicius
title: Eventflow: a functional approach to aggregates
toc: yes
---

Modelling an aggregate
========

An aggregate in a DDD system defines a consistency boundary for the entities it describes. Usually, this is implemented as a set of entities, where one of them is denoted as the main one - it is the aggregate root. To the outside world, this aggregate root is more or less a black box. No entity below the aggregate root can be referenced by other aggregates. This is required to ensure that the aggregate can maintain the children entities consistent at all times.

Such approach to model aggregates is good, and it indeed works well for many applications. One downside of it, however, in an Event Sourced application, is that it splits the model into two logical components: the current state and aggregate events. When implementing an aggregate, developers need to think in both the terms of the concrete state and the flow of aggregate events. This also usually requires a considerable amount of code to support it. An aggregate is loaded from the past events, where all entities restore their data by providing event handlers. Then, when a new command is being processed, the command handler checks the state data and emits new events.

So what can we do?
========================

The few goals of this experiment are:

  - bring the state changes and the code that uses the state closer;
  - reduce the explicit usage of state so that it allows developers to focus on the events more;
  - minimise the amount of code required to describe an aggregate.

As it happens, functional programming can help! It allows the expressivity to define command handlers that change according to the flow of aggregate events. When a new event is created, the affected command handlers are replaced by new ones. These new command handlers process the same commands, but they already know that the event has occurred and do not need to check the stored state.

How does it look like?
======================

For the usage the example I'll be writing a very simple aggregate - a counter that can be incremented or decremented, but can never go below zero.

The aggregate is defined by a list of possible flows. Each flow sets possible commands handler, if it is available at that point, and an event handler. The first command handler that handles the command for the aggregate wins, and no other command handlers will be checked. However, when an event comes, all current event listeners will be notified.

Having a list flows is useful when we want do describe a complex aggregate, where the handlers can fork into separate flows. Once a flow reaches the end of its life - by not defining a `waitFor` event listener - it is removed from the list altogether.

````scala
val aggregateLogic: List[Flow[Unit]] = List(
  handler {case Create(id) => emitEvent(Created(id))} >> 
    waitFor {case Created(_) => ()},
  waitFor {case Created(_) => ()} >>
    countingLogic(0)
)
````

The above example defines two flows. The first of them sets a command handler to allow a counter being created and is finished once the created event is emitted. The second flow starts with no defined handler. First, it waits for the aggregate to be created. Once that happens, it passes the execution to the counter logic.

Note, while the purpose of the example above was to show the usage of several flows for an aggregate, it could also be refactored to one flow. The switch to the counter logic could have been set in the first flow, just after the aggregate creation event is received.

The counter logic is slightly more complex. It shows how to transform an aggregate state to a local variable `c`. The function starts by defining a handler for the current state. The `Increment` command is allowed at all times, however, the `Decrement` command will only be successful if the current counter is above zero.

Once the handler is defined, the flow waits for the counting events to happen and then recurses to the same flow definition with the adjusted state.

````scala
def countingLogic(c: Int): Flow[Unit] =
  handler {
    case Increment => emitEvent(Incremented)
    case Decrement => if (c > 0) emitEvent(Decremented)
                      else failCommand("Counter cannot be decremented")
  } >>
  waitFor {
    case Incremented => c + 1
    case Decremented => c - 1
  } >>=
  countingLogic
````

As the flow is defined by the `Flow` monad, we could as well have used the _for comprehension_, however due to Scala's noise to signal ratio in them I've chosen the bind operators from the [cats](https://github.com/non/cats) library instead.


How does it work?
=================

The main element - the `Flow` monad is defined as a free monad using the excellent cats library. Each definition builds a lazy, recursive data structure from just a few operations - they set up the command and the event handlers.

Perhaps the most complex bit is the consumption of such structure when processing aggregate commands and events. To make this easier, the several aggregate definition steps are flattened to the following:

````scala
case class EventStreamConsumer(
    cmdh: CommandH,
    evh: Evt => Option[EventStreamConsumer]
    )
````

The structure above both gives a command handler at all times and also provides an event handler. When an event is handled successfully, the whole `EventStreamConsumer` is replaced by the returned continuation. When the event handler returns `None`, then the continuation is not possible and the flow is finished.

The code snippet below shows how is the flattening structure translation.

````scala
def esRunnerCompiler[A](initCmdH: CommandH)(esRunner: Flow[A]): Option[EventStreamConsumer] =
  esRunner.fold(
    _ => None,
    {
      case SetCommandHandler(cmdh, next) => esRunnerCompiler(cmdh)(next)
      case EventHandler(evth, cont) => {
        lazy val self: EventStreamConsumer = EventStreamConsumer(
          initCmdH,
          (ev: Evt) => evth.lift(ev) map (cont andThen esRunnerCompiler(initCmdH)) getOrElse Some(self)
        )
        Some(self)
      }
    }
  )
````

The compilation function is initially called with an empty command handler. All later `SetCommandHandler` actions override it until  an `EventHandler` action is met. At this point, the compilation is suspended and a new `EventStreamConsumer` is constructed. The interpretation of the input actions will resume once the next event is processed.

The final bits
=======

While the counter example is very minimalistic, it can show how to couple the aggregate state and the logic that uses it coherently. This way there is less context switching required from developers to define an aggregate. The resulting code is concise and easy to  reason about.

Another benefit of such approach is that the code execution is separated from the aggregate event flow definition, and both can be extended independently.

In future, I will try to describe more complex aggregates using the Event Flow. The main elements I will explore next are the flow forking and debugging a system defined in such way.

But more importantly, what do you think of such approach? Does it help to solve the problems it aims to help with? Are the problems  it is solving relevant for you?

