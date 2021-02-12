---
layout: doc-page
title: "Trait Parameters"
---

Scala 3 allows traits to have parameters, just like classes have parameters.

```scala
trait Greeting(val name: String):
   def msg = s"How are you, $name"

class C extends Greeting("Bob"):
   println(msg)
```

Arguments to a trait are evaluated immediately before the trait is initialized.

One potential issue with trait parameters is how to prevent
ambiguities. For instance, you might try to extend `Greeting` twice,
with different parameters.

```scala
class D extends C, Greeting("Bill") // error: parameter passed twice
```

Should this print "Bob" or "Bill"? In fact this program is illegal,
because it violates the second rule of the following for trait parameters:

 1. If a class `C` directly extends a parameterized trait `T`, and its superclass does not, `C` _must_ pass arguments to `T`.

 2. If a class `C` directly or indirectly extends a parameterized trait `T`, and its superclass does as well, `C` _must not_  pass arguments to `T`.

 3. Traits must never pass arguments to parent traits.

 4. If a class `C` extends a parameterized trait `T` only indirectly, and its superclass does not extend `T`, then all parameters of `T` must be defined via overrides.

Here's a trait extending the parameterized trait `Greeting`.

```scala
trait FormalGreeting extends Greeting:
   override def msg = s"How do you do, $name"
```
As is required, no arguments are passed to `Greeting`. However, this poses an issue
when defining a class that extends `FormalGreeting`:

```scala
class E extends FormalGreeting // error: missing arguments for `Greeting`.
```

The correct way to write `E` is to extend both `Greeting` and
`FormalGreeting` (in either order):

```scala
class E extends Greeting("Bob"), FormalGreeting
```
Alternatively, a class could also define the `name` parameter of `Greeting` using
an override, using rule (4) above:

```scala
class E2 extends FormalGreeting:
   override val name: String = "Bob"
```

## Reference

For more information, see [Scala SIP 25](http://docs.scala-lang.org/sips/pending/trait-parameters.html).
