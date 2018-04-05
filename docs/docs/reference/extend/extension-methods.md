---
layout: doc-page
title: "Extension Methods"
---

Extension methods allow one to add methods to a type after the type is defined. Example:

```scala
case class Circle(x: Double, y: Double, radius: Double)

extension CircleOps for Circle {
  def circumference: Double = this.radius * math.Pi * 2
}
```

The extension adds a method `circumference` to values of class `Circle`. Like regular methods, extension methods can be invoked with infix `.`:

```scala
  val circle = Circle(0, 0, 1)
  circle.circumference
```

### Meaning of `this`

Inside an extension method, the name `this` stands for the receiver on which the
method is applied when it is invoked. E.g. in the application of `circle.circumference`,
the `this` in the body of `circumference` refers to `circle`. As usual, `this` can be elided, so we could also have defined `CircleOps` like this:

```scala
extension CircleOps for Circle {
  def circumference: Double = radius * math.Pi * 2
}
```

### Scope of Extensions

Extensions can appear anywhere in a program; there is no need to co-define them with the types they extend. Extension methods are available wherever their defining extension is in scope.  Extensions can be inherited or imported like normal definitions.

### Extended Types

An extension can add methods to arbitrary types. For instance, the following
clause adds a `longestStrings` extension method to a `Seq[String]`:

```scala
extension StringOps for Seq[String] {
  def longestStrings: Seq[String] = {
    val maxLength = map(_.length).max
    filter(_.length == maxLength)
  }
}
```

### Generic Extensions

The previous example extended a specific instance of a generic type. It is also possible
to extend a generic type by adding type parameters to an extension:

```scala
extension ListOps[T] for List[T] {
  def second: T = tail.head
}
```

or:


```scala
extension ListListOps[T] for List[List[T]] {
  def flattened: List[T] = foldLeft[List[T]](Nil)(_ ++ _)
}
```

### Bounded Generic Extensions

It is possible to use bounds for the type parameters of an extension. But subtype and context bounds are supported:

```scala
extension ShapeListOps[T <: Shape] for List[T] {
  def totalArea = map(_.area).sum
}

extension OrdSeqOps[T : math.Ordering] for Seq[T] {
  def indexOfLargest  = zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest = zipWithIndex.minBy(_._1)._2
}
```

### Implicit Parameters for Type Patterns

The standard translation of context bounds expands the bound in the last example to an implicit _evidence_ parameter of type `math.Ordering[T]`. It is also possible to give evidence parameters explicitly. The following example is equivalent to the previous one:

```scala
extension OrdSeqOps[T](implicit ev: math.Ordering[T]) for Seq[T] {
  def indexOfLargest  = this.zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest = this.zipWithIndex.minBy(_._1)._2
}
```

There can be only one parameter clause following a type pattern and it must be implicit. As usual, one can combine context bounds and implicit evidence parameters.

### Toplevel Type Variables

A type pattern consisting of a top-level typevariable introduces a fully generic extension. For instance, the following extension introduces `x ~ y` as an alias
for `(x, y)`:

```scala
extension InfixPair[T] for T {
  def ~ [U](that: U) = (this, that)
}
```

As a larger example, here is a way to define constructs for checking arbitrary postconditions using `ensuring` so that the checked result can be referred to simply by `result`. The example combines opaque aliases, implicit function types, and extensions to provide a zero-overhead abstraction.

```scala
object PostConditions {
  opaque type WrappedResult[T] = T

  private object WrappedResult {
    def wrap[T](x: T): WrappedResult[T] = x
    def unwrap[T](x: WrappedResult[T]): T = x
  }

  def result[T](implicit er: WrappedResult[T]): T = WrappedResult.unwrap(er)

  extension Ensuring[T] for T {
    def ensuring(condition: implicit WrappedResult[T] => Boolean): T = {
      implicit val wrapped = WrappedResult.wrap(this)
      assert(condition)
      this
    }
  }
}
object Test {
  import PostConditions._
  val s = List(1, 2, 3).sum.ensuring(result == 6)
}
```
**Explanations**: We use an implicit function type `implicit WrappedResult[T] => Boolean`
as the type of the condition of `ensuring`. An argument condition to `ensuring` such as
`(result == 6)` will therefore have an implicit value of type `WrappedResult[T]` in scope
to pass along to the `result` method. `WrappedResult` is a fresh type, to make sure that we do not get unwanted implicits in scope (this is good practice in all cases where implicit parameters are involved). Since `WrappedResult` is an opaque type alias, its values need not be boxed, and since `ensuring` is added as an extension method, its argument does not need boxing either. Hence, the implementation of `ensuring` is as about as efficient as the best possible code one could write by hand:

    { val result = List(1, 2, 3).sum
      assert(result == 6)
      result
    }
