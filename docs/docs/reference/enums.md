---
layout: doc-page
title: "Enumerations"
---

An enumeration is used to define a type consisting of a set of named values.

```scala
enum Color {
  case Red, Green, Blue
}
```

This defines a new `sealed` class, `Color`, with three values, `Color.Red`,
`Color.Green`, `Color.Blue`.  The color values are members of `Color`s
companion object. The `Color` definition above is equivalent to the
following more explicit definition of an _enum class_ and a companion
object:

```scala
enum class Color
object Color {
  case Red
  case Green
  case Blue
}
```

### Parameterized enums

Enum classes can be parameterized.

```scala
enum Color(val rgb: Int) {
  case Red   extends Color(0xFF0000)
  case Green extends Color(0x00FF00)
  case Blue  extends Color(0x0000FF)
}
```

As the example shows, you can define the parameter value by using an
explicit extends clause.

### Methods defined for enums

The values of an enum correspond to unique integers. The integer
associated with an enum value is returned by its `enumTag` method:

```scala
scala> val red = Color.Red
val red: Color = Red
scala> red.enumTag
val res0: Int = 0
```

The companion object of an enum class also defines three utility methods.
The `enumValue` and `enumValueNamed` methods obtain an enum value
by its tag or its name. The `enumValues` method returns all enum values
defined in an enumeration in an `Iterable`.

```scala
scala> Color.enumValue(1)
val res1: Color = Green
scala> Color.enumValueNamed("Blue")
val res2: Color = Blue
scala> Color.enumValues
val res3: collection.Iterable[Color] = MapLike(Red, Green, Blue)
```

### User-defined members of enums

It is possible to add your own definitions to an enum class or its
companion object.  To make clear what goes where you need to use the
longer syntax which defines an enum class alongside its companion
object explicitly. In the following example, we define some methods in
class `Planet` and a `main` method in its companion object.

```scala
enum class Planet(mass: Double, radius: Double) {
  private final val G = 6.67300E-11
  def surfaceGravity = G * mass / (radius * radius)
  def surfaceWeight(otherMass: Double) =  otherMass * surfaceGravity
}

object Planet {
  case MERCURY extends Planet(3.303e+23, 2.4397e6)
  case VENUS   extends Planet(4.869e+24, 6.0518e6)
  case EARTH   extends Planet(5.976e+24, 6.37814e6)
  case MARS    extends Planet(6.421e+23, 3.3972e6)
  case JUPITER extends Planet(1.9e+27,   7.1492e7)
  case SATURN  extends Planet(5.688e+26, 6.0268e7)
  case URANUS  extends Planet(8.686e+25, 2.5559e7)
  case NEPTUNE extends Planet(1.024e+26, 2.4746e7)

  def main(args: Array[String]) = {
    val earthWeight = args(0).toDouble
    val mass = earthWeight/EARTH.surfaceGravity
    for (p <- enumValues)
      println(s"Your weight on $p is ${p.surfaceWeight(mass)}")
  }
}
```

### Implementation

Enum classes are represented as `sealed` classes that extend the `scala.Enum` trait.
This trait defines a single method, `enumTag`:

```scala
package scala

/** A base trait of all enum classes */
trait Enum {

  /** A number uniquely identifying a case of an enum */
  def enumTag: Int
}
```

Enum values with `extends` clauses get expanded to anonymus class instances.
For instance, the `VENUS` value above would be defined like this:

```scala
val VENUS: Planet =
  new Planet(4.869E24, 6051800.0) {
    def enumTag: Int = 1
    override def toString: String = "VENUS"
    // internal code to register value
  }
```

Enum values without `extends` clauses all share a single implementation
that can be instantiated using a private method that takes a tag and a name as arguments.
For instance, the first
definition of value `Color.Red` above would expand to:

```scala
val Red: Color = $new(0, "Red")
```

### Reference

For more info, see [Issue #1970](https://github.com/lampepfl/dotty/issues/1970).
