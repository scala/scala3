---
layout: doc-page
title: "Trait Parameters"
---

Dotty allows traits to have parameters, just like classes have parameters.

```scala
trait Greeting(val name: String) {
  def msg = s"How are you, $name"
}

class C extends Greeting("Bob") {
  println(msg)
}
```

Arguments to a trait are evaluated immediately before the trait is initialized.

One potential issue with trait parameters is how to prevent
ambiguities. For instance, you might try to extend `Greeting` twice,
with different parameters.

```scala
/*!*/ class D extends C with Greeting("Bill") // error: parameters passed twice
```

Should this print "Bob" or "Bill"? In fact this program is illegal,
because it violates one of the following rules for trait parameters:

 1. If a class `C` extends a parameterized trait `T`, and its superclass does not, `C` _must_ pass arguments to `T`.

 2. If a class `C` extends a parameterized trait `T`, and its superclass does as well, `C` _may not_  pass arguments to `T`.

 3. Traits may never pass arguments to parent traits.

Here's a trait extending the parameterized trait `Greeting`.

```scala
trait FormalGreeting extends Greeting {
  override def msg = s"How do you do, $name"
}
```
As is required, no arguments are passed to `Greeting`. However, this poses an issue
when defining a class that extends `FormalGreeting`:

```scala
/*!*/ class E extends FormalGreeting // error: missing arguments for `Greeting`.
```

The correct way to write `E` is to extend both `Greeting` and
`FormalGreeting` (in either order):

```scala
class E extends Greeting("Bob") with FormalGreeting
```

### Reference

For more info, see [Scala SIP 25](http://docs.scala-lang.org/sips/pending/trait-parameters.html).