---
layout: doc-page
title: "Method Augmentations"
---

Method augmentations are a way to define _extension methods_. Here is a simple one:

```scala
case class Circle(x: Double, y: Double, radius: Double)

augment Circle {
  def circumference: Double = this.radius * math.Pi * 2
}
```

The `augment Circle` clause adds an extension method `circumference` to values of class `Circle`. Like regular methods, extension methods can be invoked with infix `.`:

```scala
  val circle = Circle(0, 0, 1)
  circle.circumference
```

### Meaning of `this`

Inside an extension method, the name `this` stands for the receiver on which the
method is applied when it is invoked. E.g. in the application of `circle.circumference`,
the `this` in the body of `circumference` refers to `circle`. Unlike for regular methods,
an explicit `this` is mandatory to refer to members of the receiver. So the following
gives a compilation error:

```scala
   |    def circumference = radius * math.Pi * 2
   |                        ^^^^^^
   |                        not found: radius
```

### Scope of Augment Clauses

Augment clauses can appear anywhere in a program; there is no need to co-define them with the types they augment. Extension methods are available whereever their defining augment clause is in scope.  Augment clauses can be inherited or wildcard-imported like normal definitions. This is usually sufficient to control their visibility. If more control is desired, one can also attach a name to an augment clause, like this:

```scala
package shapeOps

augment circleOps @ Circle {
  def circumference: Double = this.radius * math.Pi * 2
  def area: Double = this.radius * this.radius * math.Pi
}
```
Labelled augments can be imported individually by their name:

```scala
import shapeOps.circleOps    // makes circumference and area available
```

### Augmented Types

An augment clause may add methods to arbitrary types. For instance, the following
clause adds a `longestStrings` extension method to a `Seq[String]`:

```scala
augment Seq[String] {
  def longestStrings: Seq[String] = {
    val maxLength = this.map(_.length).max
    this.filter(_.length == maxLength)
  }
}
```

### Augmented Type Patterns

The previous example augmented a specific instance of a generic type. It is also possible
to augment a generic type itself, using a _type pattern_:

```scala
augment List[type T] {
  def second: T = this.tail.head
}
```

The `type T` argument indicates that the augment applies to `List[T]`s for any type `T`.
We also say that `type T` introduces `T` as a _variable_ in the type pattern `List[type T]`.
Type variables may appear anywhere in a type pattern. Example:

```scala
augment List[List[type T]] {
  def flattened: List[T] = this.foldLeft[List[T]](Nil)(_ ++ _)
}
```

### Type Patterns in Cases

The `type ...` syntax for pattern bound type variables also applies to patterns in
case clauses of `match` and `try` expressions. For instance:

```scala
def f[T](s: Set[T], x: T) = s match {
  case _: SortedSet[type U] => ... // binds `U`, infers that `U = T`
  case _ =>
}
```

Previously, one used a lower-case name to indicate a variable in a type pattern, as in:

```scala
  case _: SortedSet[u] => ... // binds `u`, infers that `u = T`
```

While being more regular wrt term variables in patterns, this usage is harder to read, and has the problem that it feels unnatrual to have to write type names in lower case. It will therefore be phased out to be replaced by the explicit `type T` syntax.

Type patterns in cases only come in unbounded form; the bounds defined in the next section are not applicable to them.

### Bounds in Augmented Type Patterns

It is also possible to use bounds for the type variables in an augmented type pattern. Examples:

```scala
augment List[type T <: Shape] {
  def totalArea = this.map(_.area).sum
}
```

Context-bounds are also supported:

```scala
augment Seq[type T: math.Ordering] {
  def indexOfLargest  = this.zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest = this.zipWithIndex.minBy(_._1)._2
}
```

### Implicit Parameters for Type Patterns

The standard translation of context bounds expands the bound in the last example to an implicit _evidence_ parameter of type `math.Ordering[T]`. It is also possible to give evidence parameters explicitly. The following example is equivalent to the previous one:

```scala
augment Seq[type T](implicit ev: math.Ordering[T]) {
  def indexOfLargest  = this.zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest = this.zipWithIndex.minBy(_._1)._2
}
```

There can be only one parameter clause following a type pattern and it must be implicit. As usual, one can combine context bounds and implicit evidence parameters.

### Toplevel Type Variables

A type pattern consisting of a top-level typevariable introduces a fully generic augmentation. For instance, the following augment introduces `x ~ y` as an alias
for `(x, y)`:

```scala
augment (type T) {
  def ~ [U](that: U) = (this, that)
}
```

As a larger example, here is a way to define constructs for checking arbitrary postconditions using `ensuring` so that the checked result can be referred to simply by `result`. The example combines opaque aliases, implicit function types, and augments to provide a zero-overhead abstraction.

```scala
object PostConditions {
  opaque type WrappedResult[T] = T

  private object WrappedResult {
    def wrap[T](x: T): WrappedResult[T] = x
    def unwrap[T](x: WrappedResult[T]): T = x
  }

  def result[T](implicit er: WrappedResult[T]): T = WrappedResult.unwrap(er)

  augment (type T) {
    def ensuring[U](condition: implicit WrappedResult[T] => Boolean): T = {
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
