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
companion object.

### Parameterized enums

Enums can be parameterized.

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
associated with an enum value is returned by its `ordinal` method:

```scala
scala> val red = Color.Red
val red: Color = Red
scala> red.ordinal
val res0: Int = 0
```

The companion object of an enum also defines two utility methods.
The `valueOf` method obtains an enum value
by its name. The `values` method returns all enum values
defined in an enumeration in an `Array`.

```scala
scala> Color.valueOf("Blue")
val res0: Color = Blue
scala> Color.values
val res1: Array[Color] = Array(Red, Green, Blue)
```

### User-defined members of enums

It is possible to add your own definitions to an enum. Example:

```scala
enum Planet(mass: Double, radius: Double) {
  private final val G = 6.67300E-11
  def surfaceGravity = G * mass / (radius * radius)
  def surfaceWeight(otherMass: Double) =  otherMass * surfaceGravity

  case Mercury extends Planet(3.303e+23, 2.4397e6)
  case Venus   extends Planet(4.869e+24, 6.0518e6)
  case Earth   extends Planet(5.976e+24, 6.37814e6)
  case Mars    extends Planet(6.421e+23, 3.3972e6)
  case Jupiter extends Planet(1.9e+27,   7.1492e7)
  case Saturn  extends Planet(5.688e+26, 6.0268e7)
  case Uranus  extends Planet(8.686e+25, 2.5559e7)
  case Neptune extends Planet(1.024e+26, 2.4746e7)
}
```

It is also possible to define an explicit companion object for an enum:

```scala
object Planet {
  def main(args: Array[String]) = {
    val earthWeight = args(0).toDouble
    val mass = earthWeight / Earth.surfaceGravity
    for (p <- values)
      println(s"Your weight on $p is ${p.surfaceWeight(mass)}")
  }
}
```

### Compatibility with Java Enums
If you want to use the Scala-defined enums as Java enums, you can do so by extending `compat.JEnum` class as follows:

```scala
enum Color extends compat.JEnum[Color] { case Red, Green, Blue }
```

The type parameter comes from the Java enum [definition](https://docs.oracle.com/javase/8/docs/api/index.html?java/lang/Enum.html) and should me the same as the type of the enum. The compiler will transform the definition above so that `Color` extends `java.lang.Enum`.

After defining `Color` like that, you can use like you would a Java enum:

```scala
scala> Color.Red.compareTo(Color.Green)
val res15: Int = -1
```

### Implementation

Enums are represented as `sealed` classes that extend the `scala.Enum` trait.
This trait defines a single public method, `ordinal`:

```scala
package scala

/** A base trait of all enum classes */
trait Enum {

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int
}
```

Enum values with `extends` clauses get expanded to anonymous class instances.
For instance, the `Venus` value above would be defined like this:

```scala
val Venus: Planet =
  new Planet(4.869E24, 6051800.0) {
    def ordinal: Int = 1
    override def toString: String = "Venus"
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

For more info, see [Issue #1970](https://github.com/lampepfl/dotty/issues/1970) and
[PR #4003](https://github.com/lampepfl/dotty/pull/4003).
