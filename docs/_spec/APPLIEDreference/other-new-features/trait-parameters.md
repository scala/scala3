---
layout: doc-page
title: "Trait Parameters"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/trait-parameters.html
---

Scala 3 enables traits to have parameters, just like a class.

For example, here is a trait `Greeting`:
```scala
trait Greeting(val name: String):
  def msg = s"How are you, $name"
```

A class, enum, or object can extend `Greeting` as follows:

```scala
class Greet extends Greeting("Bob"):
  println(msg)
```

However if another trait extends `Greeting` then it must not pass arguments:

```scala
trait FormalGreeting extends Greeting:
  override def msg = s"How do you do, $name"
```

If you want a class to greet Bob formally, then you should extend both `FormalGreeting` and `Greeting`:

```scala
class GreetFormally extends FormalGreeting, Greeting("Bob"):
  println(msg)
```
