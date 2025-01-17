---
layout: doc-page
title: "Collection Literals"
redirectFrom: /docs/reference/other-new-features/collection-literals.html
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/collection-literals.html
---


Support for collection literals is enabled by the experimental language import
```scala
import scala.language.experimental.collectionLiterals
```
This feature requires a source version 3.7 or higher. One can specify both import and source version on the command line with these settings:
```
 -source 3.7 -language:experimental.collectionLiterals
```
Collection literals are comma-separated sequences of expressions, like these:
```scala
  val oneTwoThree = [1, 2, 3]
  val anotherLit  = [math.Pi, math.cos(2.0), math.E * 3.0]
  val diag        = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
  val empty       = []
  val mapy        = [1 -> "one", 2 -> "two", 3 -> "three"]
```
The type of a collection literal depends on the expected type. If there is no expected type (as in the examples above) a collection literal is of type `Seq`, except if it consists exclusively elements of the form `a -> b`, then it is of type `Map`. For instance, the literals above would
get inferred types as follows.
```scala
  val oneTwoThree: Seq[Int]   = [1, 2, 3]
  val anotherLit: Seq[Double] = [math.Pi, math.cos(2.0), math.E * 3.0]
  val diag: Seq[Seq[Int]]     = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
  val empty: Seq[Nothing]     = []
  val mapy: Map[Int, String]  = [1 -> "one", 2 -> "two", 3 -> "three"]
```
If there is an expected type `E`, the compiler will search for a given instance of the
type class `ExpressibleAsCollectionLiteral[E]`. This type class is defined in package `scala.compiletime` as follows:
```scala
  trait ExpressibleAsCollectionLiteral[+Coll]:

    /** The element type of the created collection */
    type Elem

    /** The inline method that creates the collection */
    inline def fromLiteral(inline xs: Elem*): Coll
```
If a best matching instance `ecl` is found, its `fromLiteral` method is used to convert
the elements of the literal to the expected type. If the search is ambiguous, it will be
reported as an error. If no matching instance is found, the literal will be typed by the default scheme as if there was no expected type.

The companion object of `ExpressibleAsCollectionLiteral` contains a number of given instances for standard collection types. For instance, there is:
```scala
  given vectorFromLiteral: [T] => ExpressibleAsCollectionLiteral[Vector[T]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*) = Vector[Elem](xs*)
```
Hence, the definition
```scala
  val v: Vector[Int] = [1, 2, 3]
```
would be expanded by the compiler to
```scala
  val v: Vector[Int] = vectorFromLiteral.fromLiteral(1, 2, 3)
```
After inlining, this produces
```scala
  val v: Vector[Int] = Vector[Int](1, 2, 3)
```
Using this scheme, the literals we have seen earlier could also be given alternative types like these:
```scala
  val oneTwoThree: Vector[Int]   = [1, 2, 3]
  val anotherLit: IArray[Double] = [math.Pi, math.cos(2.0), math.E * 3.0]
  val diag: Array[Array[Int]]    = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
  val empty: ArrayBuffer[Object] = []
  val mapy: HashMap[Int, String] = [1 -> "one", 2 -> "two", 3 -> "three"]
```

**Notes**

 - Since the fromLiteral method in `ExpressibleAsCollectionLiteral` is an inline method with inline arguments, given instances can implement it as a macro.

 - The precise meaning of "is there an expected type?" is as follows: There is no expected
   type if the expected type known from the context is _under-specified_, as it is defined for
   implicit search. That is, an implicit search for a given of the type would not be
   attempted because the type is not specific enough. Concretely, this is the case for Wildcard types `?`, `Any`, `AnyRef`, unconstrained type variables, or type variables constrained from above by an under-specified type.

 - If the expected type is a subtype of `Seq` or an array type, we typecheck the
   elements with the elements of the expected type. This means we can get the same
   precision in propagated expected types as if the constructor was written explicitly.
   Hence, we can't regress by going from `Seq(...)` or `Array(...)` to a
   collection literal.

**Syntax**

```
SimpleExpr       ::=  ...
                   |  ‘[’ ExprInParens {‘,’ ExprInParens} ‘]’
```
