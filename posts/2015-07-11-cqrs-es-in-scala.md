---
author: Sarunas Valaskevicius
title: CQRS/ES in Scala: an overview
toc: yes
---

# Where are we now?

[Last time](/posts/cqrs-es-in-haskell/) we've looked at implementing a CQRS/ES application in Haskell - a purely functional programming language. This gave us some insights of how we can compose the elements together and we'll use this knowledge today. Today we're going to look at some CQRS/ES implementations in Scala.


# CQRS with Akka actors and functional domain models (01/2011)

This is one of the first examples of CQRS in scala.

 - [blog](http://debasishg.blogspot.co.uk/2011/01/cqrs-with-akka-actors-and-functional.html)
 - [github](https://github.com/debasishg/cqrs-akka)

# Akka DDDD template (03/2015)

- [github](https://github.com/boldradius/akka-dddd-template#master)

# Reactive DDD with Akka (03/2015)

- [blog](http://pkaczor.blogspot.co.uk/2014/04/reactive-ddd-with-akka.html)
- [github](https://github.com/pawelkaczor/ddd-leaven-akka-v2)

# CQRS template (04/2015)

- [github](https://github.com/jgordijn/cqrs_template) -

# Final thoughts
- They all use Akka to model an aggregate.
