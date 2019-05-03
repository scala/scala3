---
layout: doc-page
title: "Opaque Type Aliases"
---

Opaque types aliases provide type abstraction without any overhead. Example:

```scala
opaque type Logarithm = Double
```

This introduces `Logarithm` as a new type, which is implemented as `Double` but is different from it. The fact that `Logarithm` is the same as `Double` is only known in the companion object of `Logarithm`. Here is a possible companion object:

```scala
object Logarithm {

  // These are the ways to lift to the logarithm type
  def apply(d: Double): Logarithm = math.log(d)

  def safe(d: Double): Option[Logarithm] =
    if (d > 0.0) Some(math.log(d)) else None

  // This is the first way to unlift the logarithm type
  def exponent(l: Logarithm): Double = l

  // Extension methods define opaque types' public APIs
  implicit class LogarithmOps(val `this`: Logarithm) extends AnyVal {
    // This is the second way to unlift the logarithm type
    def toDouble: Double = math.exp(`this`)
    def +(that: Logarithm): Logarithm = Logarithm(math.exp(`this`) + math.exp(that))
    def *(that: Logarithm): Logarithm = Logarithm(`this` + that)
  }
}
```

The companion object contains with the `apply` and `safe` methods ways to convert from doubles to `Logarithm` values. It also adds an `exponent` function and a decorator that implements `+` and `*` on logarithm values, as well as a conversion `toDouble`. All this is possible because within object `Logarithm`, the type `Logarithm` is just an alias of `Double`.

Outside the companion object, `Logarithm` is treated as a new abstract type. So the
following operations would be valid because they use functionality implemented in the `Logarithm` object.

```scala
  val l = Logarithm(1.0)
  val l2 = Logarithm(2.0)
  val l3 = l * l2
  val l4 = l + l2
```

But the following operations would lead to type errors:

```scala
  val d: Double = l       // error: found: Logarithm, required: Double
  val l2: Logarithm = 1.0 // error: found: Double, required: Logarithm
  l * 2                   // error: found: Int(2), required: Logarithm
  l / l2                  // error: `/` is not a member fo Logarithm
```

`opaque` is a [soft modifier](../soft-modifier.html).

For more details, see [Scala SIP 35](https://docs.scala-lang.org/sips/opaque-types.html).
