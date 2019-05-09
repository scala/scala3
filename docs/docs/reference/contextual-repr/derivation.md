---
layout: doc-page
title: Typeclass Derivation
---

Typeclass derivation is a way to generate representatives of certain type classes automatically or with minimal code hints. A type class in this sense is any trait or class with a type parameter that describes the type being operated on. Commonly used examples are `Eql`, `Ordering`, `Show`, or `Pickling`. Example:
```scala
enum Tree[T] derives Eql, Ordering, Pickling {
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)
}
```
The `derives` clause generates representatives of the `Eql`, `Ordering`, and `Pickling` traits in the companion object `Tree`:
```scala
repr [T: Eql]      of Eql[Tree[T]]       = Eql.derived
repr [T: Ordering] of Ordering[Tree[T]] = Ordering.derived
repr [T: Pickling] of Pickling[Tree[T]] = Pickling.derived
```

### Deriving Types

Besides for enums, typeclasses can also be derived for other sets of classes and objects that form an algebraic data type. These are:

 - individual case classes or case objects
 - sealed classes or traits that have only case classes and case objects as children.

 Examples:

 ```scala
case class Labelled[T](x: T, label: String) derives Eql, Show

sealed trait Option[T] derives Eql
case class Some[T] extends Option[T]
case object None extends Option[Nothing]
```

The generated typeclass representatives are placed in the companion objects `Labelled` and `Option`, respectively.

### Derivable Types

A trait or class can appear in a `derives` clause if its companion object defines a method named `derived`. The type and implementation of a `derived` method are arbitrary, but typically it has a definition like this:
```scala
  def derived[T] given Generic[T] = ...
```
That is, the `derived` method takes an implicit parameter of type `Generic` that determines the _shape_ of the deriving type `T` and it computes the typeclass implementation according to that shape. A `Generic` representative is generated automatically for any type that derives a typeclass with a `derived` method that refers to `Generic`. One can also derive `Generic` alone, which means a `Generic` representative is generated without any other type class representatives. E.g.:
```scala
sealed trait ParseResult[T] derives Generic
```
This is all a user of typeclass derivation has to know. The rest of this page contains information needed to be able to write a typeclass that can appear in a `derives` clause. In particular, it details the means provided for the implementation of data generic `derived` methods.

### The Shape Type

For every class with a `derives` clause, the compiler computes the shape of that class as a type. For example, here is the shape type for the `Tree[T]` enum:
```scala
Cases[(
  Case[Branch[T], (Tree[T], Tree[T])],
  Case[Leaf[T], T *: Unit]
)]
```
Informally, this states that

> The shape of a `Tree[T]` is one of two cases: Either a `Branch[T]` with two
  elements of type `Tree[T]`, or a `Leaf[T]` with a single element of type `T`.

The type constructors `Cases` and `Case` come from the companion object of a class
`scala.compiletime.Shape`, which is defined in the standard library as follows:
```scala
sealed abstract class Shape

object Shape {

  /** A sum with alternative types `Alts` */
  case class Cases[Alts <: Tuple] extends Shape

  /** A product type `T` with element types `Elems` */
  case class Case[T, Elems <: Tuple] extends Shape
}
```

Here is the shape type for `Labelled[T]`:
```scala
Case[Labelled[T], (T, String)]
```
And here is the one for `Option[T]`:
```scala
Cases[(
  Case[Some[T], T *: Unit],
  Case[None.type, Unit]
)]
```
Note that an empty element tuple is represented as type `Unit`. A single-element tuple
is represented as `T *: Unit` since there is no direct syntax for such tuples: `(T)` is just `T` in parentheses, not a tuple.

### The Generic Typeclass

For every class `C[T_1,...,T_n]` with a `derives` clause, the compiler generates in the companion object of `C` a representative of `Generic[C[T_1,...,T_n]]` that follows the outline below:
```scala
repr [T_1, ..., T_n] of Generic[C[T_1,...,T_n]] {
  type Shape = ...
  ...
}
```
where the right hand side of `Shape` is the shape type of `C[T_1,...,T_n]`.
For instance, the definition
```scala
enum Result[+T, +E] derives Logging {
  case class Ok[T](result: T)
  case class Err[E](err: E)
}
```
would produce:
```scala
object Result {
  import scala.compiletime.Shape._

  repr [T, E] of Generic[Result[T, E]] {
    type Shape = Cases[(
      Case[Ok[T], T *: Unit],
      Case[Err[E], E *: Unit]
    )]
    ...
  }
}
```
The `Generic` class is defined in package `scala.reflect`.

```scala
abstract class Generic[T] {
  type Shape <: scala.compiletime.Shape

  /** The mirror corresponding to ADT instance `x` */
  def reflect(x: T): Mirror

  /** The ADT instance corresponding to given `mirror` */
  def reify(mirror: Mirror): T

  /** The companion object of the ADT */
  def common: GenericClass
}
```
It defines the `Shape` type for the ADT `T`, as well as two methods that map between a
type `T` and a generic representation of `T`, which we call a `Mirror`:
The `reflect` method maps an instance of the ADT `T` to its mirror whereas
the `reify` method goes the other way. There's also a `common` method that returns
a value of type `GenericClass` which contains information that is the same for all
instances of a class (right now, this consists of the runtime `Class` value and
the names of the cases and their parameters).

### Mirrors

A mirror is a generic representation of an instance of an ADT. `Mirror` objects have three components:

 - `adtClass: GenericClass`: The representation of the ADT class
 - `ordinal: Int`: The ordinal number of the case among all cases of the ADT, starting from 0
 - `elems: Product`: The elements of the instance, represented as a `Product`.

 The `Mirror` class is defined in package `scala.reflect` as follows:

```scala
class Mirror(val adtClass: GenericClass, val ordinal: Int, val elems: Product) {

  /** The `n`'th element of this generic case */
  def apply(n: Int): Any = elems.productElement(n)

  /** The name of the constructor of the case reflected by this mirror */
  def caseLabel: String = adtClass.label(ordinal)(0)

  /** The label of the `n`'th element of the case reflected by this mirror */
  def elementLabel(n: Int): String = adtClass.label(ordinal)(n + 1)
}
```

### GenericClass

Here's the API of `scala.reflect.GenericClass`:

```scala
class GenericClass(val runtimeClass: Class[_], labelsStr: String) {

  /** A mirror of case with ordinal number `ordinal` and elements as given by `Product` */
  def mirror(ordinal: Int, product: Product): Mirror =
    new Mirror(this, ordinal, product)

  /** A mirror with elements given as an array */
  def mirror(ordinal: Int, elems: Array[AnyRef]): Mirror =
    mirror(ordinal, new ArrayProduct(elems))

  /** A mirror with an initial empty array of `numElems` elements, to be filled in. */
  def mirror(ordinal: Int, numElems: Int): Mirror =
    mirror(ordinal, new Array[AnyRef](numElems))

  /** A mirror of a case with no elements */
  def mirror(ordinal: Int): Mirror =
    mirror(ordinal, EmptyProduct)

  /** Case and element labels as a two-dimensional array.
   *  Each row of the array contains a case label, followed by the labels of the elements of that case.
   */
  val label: Array[Array[String]] = ...
}
```

The class provides four overloaded methods to create mirrors. The first of these is invoked by the `reify` method that maps an ADT instance to its mirror. It simply passes the
instance itself (which is a `Product`) to the second parameter of the mirror. That operation does not involve any copying and is thus quite efficient. The second and third versions of `mirror` are typically invoked by typeclass methods that create instances from mirrors. An example would be an `unpickle` method that first creates an array of elements, then creates
a mirror over that array, and finally uses the `reify` method in `Reflected` to create the ADT instance. The fourth version of `mirror` is used to create mirrors of instances that do not have any elements.

### How to Write Generic Typeclasses

Based on the machinery developed so far it becomes possible to define type classes generically. This means that the `derived` method will compute a type class representative for any ADT that has a `Generic` representative, recursively.
The implementation of these methods typically uses three new type-level constructs in Dotty: inline methods, inline matches, and implicit matches. As an example, here is one possible implementation of a generic `Eql` type class, with explanations. Let's assume `Eql` is defined by the following trait:
```scala
trait Eql[T] {
  def eql(x: T, y: T): Boolean
}
```
We need to implement a method `Eql.derived` that produces a representative of `Eql[T]` provided
there exists a representative of type `Generic[T]`. Here's a possible solution:
```scala
  inline def derived[T] given (ev: Generic[T]): Eql[T] = new Eql[T] {
    def eql(x: T, y: T): Boolean = {
      val mx = ev.reflect(x)                    // (1)
      val my = ev.reflect(y)                    // (2)
      inline erasedValue[ev.Shape] match {
        case _: Cases[alts] =>
          mx.ordinal == my.ordinal &&           // (3)
          eqlCases[alts](mx, my, 0)             // [4]
        case _: Case[_, elems] =>
          eqlElems[elems](mx, my, 0)            // [5]
      }
    }
  }
```
The implementation of the inline method `derived` creates a representative of `Eql[T]` and implements its `eql` method. The right-hand side of `eql` mixes compile-time and runtime elements. In the code above, runtime elements are marked with a number in parentheses, i.e
`(1)`, `(2)`, `(3)`. Compile-time calls that expand to runtime code are marked with a number in brackets, i.e. `[4]`, `[5]`. The implementation of `eql` consists of the following steps.

  1. Map the compared values `x` and `y` to their mirrors using the `reflect` method of the implicitly passed `Generic` `(1)`, `(2)`.
  2. Match at compile-time against the shape of the ADT given in `ev.Shape`. Dotty does not have a construct for matching types directly, but we can emulate it using an `inline` match over an `erasedValue`. Depending on the actual type `ev.Shape`, the match will reduce at compile time to one of its two alternatives.
  3. If `ev.Shape` is of the form `Cases[alts]` for some tuple `alts` of alternative types, the equality test consists of comparing the ordinal values of the two mirrors `(3)` and, if they are equal, comparing the elements of the case indicated by that ordinal value. That second step is performed by code that results from the compile-time expansion of the `eqlCases` call `[4]`.
  4. If `ev.Shape` is of the form `Case[elems]` for some tuple `elems` for element types, the elements of the case are compared by code that results from the compile-time expansion of the `eqlElems` call `[5]`.

Here is a possible implementation of `eqlCases`:
```scala
  inline def eqlCases[Alts <: Tuple](mx: Mirror, my: Mirror, n: Int): Boolean =
    inline erasedValue[Alts] match {
      case _: (Shape.Case[_, elems] *: alts1) =>
        if (mx.ordinal == n)                    // (6)
          eqlElems[elems](mx, my, 0)            // [7]
        else
          eqlCases[alts1](mx, my, n + 1)        // [8]
      case _: Unit =>
        throw new MatchError(mx.ordinal)        // (9)
    }
```
The inline method `eqlCases` takes as type arguments the alternatives of the ADT that remain to be tested. It takes as value arguments mirrors of the two instances `x` and `y` to be compared and an integer `n` that indicates the ordinal number of the case that is tested next. It produces an expression that compares these two values.

If the list of alternatives `Alts` consists of a case of type `Case[_, elems]`, possibly followed by further cases in `alts1`, we generate the following code:

 1. Compare the `ordinal` value of `mx` (a runtime value) with the case number `n` (a compile-time value translated to a constant in the generated code) in an if-then-else `(6)`.
 2. In the then-branch of the conditional we have that the `ordinal` value of both mirrors
 matches the number of the case with elements `elems`. Proceed by comparing the elements
 of the case in code expanded from the `eqlElems` call `[7]`.
 3. In the else-branch of the conditional we have that the present case does not match
 the ordinal value of both mirrors. Proceed by trying the remaining cases in `alts1` using
 code expanded from the `eqlCases` call `[8]`.

 If the list of alternatives `Alts` is the empty tuple, there are no further cases to check.
 This place in the code should not be reachable at runtime. Therefore an appropriate
 implementation is by throwing a `MatchError` or some other runtime exception `(9)`.

The `eqlElems` method compares the elements of two mirrors that are known to have the same
ordinal number, which means they represent the same case of the ADT. Here is a possible
implementation:
```scala
  inline def eqlElems[Elems <: Tuple](xs: Mirror, ys: Mirror, n: Int): Boolean =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryEql[elem](                           // [12]
          xs(n).asInstanceOf[elem],             // (10)
          ys(n).asInstanceOf[elem]) &&          // (11)
        eqlElems[elems1](xs, ys, n + 1)         // [13]
      case _: Unit =>
        true                                    // (14)
    }
```
`eqlElems` takes as arguments the two mirrors of the elements to compare and a compile-time index `n`, indicating the index of the next element to test. It is defined in terms of another compile-time match, this time over the tuple type `Elems` of all element types that remain to be tested. If that type is
non-empty, say of form `elem *: elems1`, the following code is produced:

 1. Access the `n`'th elements of both mirrors and cast them to the current element type `elem`
 `(10)`, `(11)`. Note that because of the way runtime reflection mirrors compile-time `Shape` types, the casts are guaranteed to succeed.
 2. Compare the element values using code expanded by the `tryEql` call `[12]`.
 3. "And" the result with code that compares the remaining elements using a recursive call
 to `eqlElems` `[13]`.

 If type `Elems` is empty, there are no more elements to be compared, so the comparison's result is `true`. `(14)`

 Since `eqlElems` is an inline method, its recursive calls are unrolled. The end result is a conjunction `test_1 && ... && test_n && true` of test expressions produced by the `tryEql` calls.

The last, and in a sense most interesting part of the derivation is the comparison of a pair of element values in `tryEql`. Here is the definition of this method:
```scala
  inline def tryEql[T](x: T, y: T) = implicit match {
    case ev: Eql[T] =>
      ev.eql(x, y)                              // (15)
    case _ =>
      error("No `Eql` instance was found for $T")
  }
```
`tryEql` is an inline method that takes an element type `T` and two element values of that type as arguments. It is defined using an `implicit match` that tries to find a representative of `Eql[T]`. If a representative `ev` is found, it proceeds by comparing the arguments using `ev.eql`. On the other hand, if no representative is found
this signals a compilation error: the user tried a generic derivation of `Eql` for a class with an element type that does not have an `Eql` representative itself. The error is signaled by
calling the `error` method defined in `scala.compiletime`.

**Note:** At the moment our error diagnostics for metaprogramming does not support yet interpolated string arguments for the `scala.compiletime.error` method that is called in the second case above. As an alternative, one can simply leave off the second case, then a missing typeclass would result in a "failure to reduce match" error.

**Example:** Here is a slightly polished and compacted version of the code that's generated by inline expansion for the derived `Eql` representative of class `Tree`.

```scala
repr [T] of Eql[Tree[T]] given (elemEq: Eql[T]) {
  def eql(x: Tree[T], y: Tree[T]): Boolean = {
    val ev = the[Generic[Tree[T]]]
    val mx = ev.reflect(x)
    val my = ev.reflect(y)
    mx.ordinal == my.ordinal && {
      if (mx.ordinal == 0) {
        this.eql(mx(0).asInstanceOf[Tree[T]], my(0).asInstanceOf[Tree[T]]) &&
        this.eql(mx(1).asInstanceOf[Tree[T]], my(1).asInstanceOf[Tree[T]])
      }
      else if (mx.ordinal == 1) {
        elemEq.eql(mx(0).asInstanceOf[T], my(0).asInstanceOf[T])
      }
      else throw new MatchError(mx.ordinal)
    }
  }
}
```

One important difference between this approach and Scala-2 typeclass derivation frameworks such as Shapeless or Magnolia is that no automatic attempt is made to generate typeclass representatives of elements recursively using the generic derivation framework. There must be a representative of `Eql[T]` (which can of course be produced in turn using `Eql.derived`), or the compilation will fail. The advantage of this more restrictive approach to typeclass derivation is that it avoids uncontrolled transitive typeclass derivation by design. This keeps code sizes smaller, compile times lower, and is generally more predictable.

### Deriving Representatives Elsewhere

Sometimes one would like to derive a typeclass representative for an ADT after the ADT is defined, without being able to change the code of the ADT itself.
To do this, simply define a representative with the `derived` method of the typeclass as right-hand side. E.g, to implement `Ordering` for `Option`, define:
```scala
repr [T: Ordering] of Ordering[Option[T]] = Ordering.derived
```
Usually, the `Ordering.derived` clause has an implicit parameter of type
`Generic[Option[T]]`. Since the `Option` trait has a `derives` clause,
the necessary representative is already present in the companion object of `Option`.
If the ADT in question does not have a `derives` clause, a `Generic` representative
would still be synthesized by the compiler at the point where `derived` is called.
This is similar to the situation with type tags or class tags: If no representative
is found, the compiler will synthesize one.

### Syntax

```
Template          ::=  InheritClauses [TemplateBody]
EnumDef           ::=  id ClassConstr InheritClauses EnumBody
InheritClauses    ::=  [‘extends’ ConstrApps] [‘derives’ QualId {‘,’ QualId}]
ConstrApps        ::=  ConstrApp {‘with’ ConstrApp}
                    |  ConstrApp {‘,’ ConstrApp}
```

### Discussion

The typeclass derivation framework is quite small and low-level. There are essentially
two pieces of infrastructure in the compiler-generated `Generic` representatives:

 - a type representing the shape of an ADT,
 - a way to map between ADT instances and generic mirrors.

Generic mirrors make use of the already existing `Product` infrastructure for case
classes, which means they are efficient and their generation requires not much code.

Generic mirrors can be so simple because, just like `Product`s, they are weakly
typed. On the other hand, this means that code for generic typeclasses has to
ensure that type exploration and value selection proceed in lockstep and it
has to assert this conformance in some places using casts. If generic typeclasses
are correctly written these casts will never fail.

It could make sense to explore a higher-level framework that encapsulates all casts
in the framework. This could give more guidance to the typeclass implementer.
It also seems quite possible to put such a framework on top of the lower-level
mechanisms presented here.
