---
author: Sarunas Valaskevicius
title: Eventflow: even more succinct now
toc: no
---

[Last time](/posts/eventflow-more-complex-example/) in the eventflow series we've seen how to define a bit more complex example. However, the example, while being functionally correct, has started to show that systems can still seem more complex than they should be. This time, we'll see a new addition to the eventflow - a custom DSL, that makes defining an aggregate a breeze!

The whole doors example described in the earlier post can be rewritten as:

```scala
def openDoors: Flow[Unit] = handler(
  when(Close).emit(Closed).switch(closedDoors)
)

def closedDoors: Flow[Unit] = handler(
  when(Open).emit(Opened),
  when[Lock].emit[Locked].switch(ev => lockedDoors(ev.key))
)

def lockedDoors(key: String): Flow[Unit] = handler(
  when(Unlock(key)).emit[Unlocked].switch(closedDoors),
  when[Unlock].failWithMessage("Attempted unlock key is invalid"),
  anyOther.failWithMessage("Locked door can only be unlocked.")
)

val fullAggregate: Flow[Unit] = handler(
  when[Register].emit[Registered].switch(openDoors)
)
```

Which is in a much more succinct form and can be understood from just a glance. The command handler for unlocking the door checks for the same key in a form of a specific command matcher. If such matcher is not enough, a custom `guard` statement is available as well, shown in the counter example:

```scala
def counting(c: Int): Flow[Unit] = handler(
  when(Increment).emit(Incremented).switch(counting(c + 1)),
  when(Decrement).guard(_ => c > 0, "Counter cannot be decremented").emit(Decremented).switch(counting(c - 1))
)
```

Moreover, the language allows a few more different variations to define a system e.g.

```scala
when(Unlock(key)).emitEvent(cmd => Unlocked(cmd.key)).switch(closedDoors), // alternative to `when(Unlock(key)).emit[Unlocked]`
on(Unlocked(key)).switch(closedDoors),                                     // alternative to `emit[Unlocked].switch(closedDoors)`
on(Unlocked(key)).switch(evt => closedDoors),                              // alternative to above
on[Unlocked].switch(evt => closedDoors),                                   // alternative to above
on[Unlocked].switch(closedDoors),                                          // alternative to above
when[Unlock].failWithMessage("Attempted unlock key is invalid"),           // match any Unlock command and fail with the given message
```

Similarly, you can set up tests focusing on the aggregate behaviour:

```scala
it should "allow locking door" in {
  given {
    newDbRunner.withEvents[Event](tag, "door", Registered("door"), Closed)
  } when {
    _.command(DoorAggregate, "door", Lock("key"))
  } thenCheck {
    _.newEvents[Event](tag, "door") should be(List(Locked("key")))
  }
}
```

I hope you like it! The next step for eventflow is to make it useful and connect to a real datastore for the events.

Have a play using the [github repo](https://github.com/svalaskevicius/eventflow) and let me know your findings!

