---
author: Sarunas Valaskevicius
title: Eventflow: a more complex example
toc: no
---

[Last time](/posts/eventflow-cqrs-es-in-scala/) we've seen an example of a simple aggregate defined as an _Eventflow_. We've created a simple counter, that can be incremented and decremented, but it cannot be decremented below zero. All it needed was a local state for the counter value and an ability to fail a command.

This time we'll define just a little bit more complex aggregate logic - a door.

- A door can be registered in the system;
- it can be closed or opened;
- if its closed, it can be locked with a key;
- if its locked, it can be unlocked with the same key.

Let's define the ADT for the door events:

````scala
sealed trait Event
final case class Registered(id: String) extends Event
case object Opened extends Event
case object Closed extends Event
final case class Locked(key: String) extends Event
final case class Unlocked(key: String) extends Event
````

And one for the commands:

````scala
sealed trait Command
final case class Register(id: String) extends Command
case object Open extends Command
case object Close extends Command
final case class Lock(key: String) extends Command
final case class Unlock(key: String) extends Command
````

The main difference from the "counter" example is that here we have to switch to a different flow based on a previous event.

The code for it is as follows:

````scala
def openDoorsLogic: Flow[Unit] =
    handler {
        case Close => emitEvent(Closed)
        case _ => failCommand("Open door can only be closed.")
    } >>
    waitForAndSwitch {
        case Closed => closedDoorsLogic
    }
````

The structure of the above code is very similar to that of the previous counter example - with the only addition: `waitForAndSwitch`. Let's see how can we use it better:

````scala
def closedDoorsLogic: Flow[Unit] =
    handler {
        case Lock(key) => emitEvent(Locked(key))
        case Open => emitEvent(Opened)
        case _ => failCommand("Closed door can only be opened or locked.")
    } >>
    waitForAndSwitch {
        case Opened => openDoorsLogic
        case Locked(key) => lockedDoorsLogic(key)
    }
````

Each event matching case specifies its own flow continuation. Also, it can pass parameters - such as the key a door was locked with.

````scala
def lockedDoorsLogic(key: String): Flow[Unit] =
    handler {
        case Unlock(attemptedKey) => if (key == attemptedKey) emitEvent(Unlocked(attemptedKey))
                                     else failCommand("Attempted to unlock with an invalid key.")
        case _ => failCommand("A locked door can only be unlocked.")
    } >>
    waitForAndSwitch {
        case Unlocked(_) => closedDoorsLogic
    }

````

With this, the door logic definition is complete. As it directly encodes the door's state machine in text, I believe it is one of the clearest and most succinct forms to describe possible changes in a system. Of course, it can still be improved, and if you have any ideas how to do that, I'd love to hear them!
