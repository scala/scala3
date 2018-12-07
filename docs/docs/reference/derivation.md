---
layout: doc-page
title: Typeclass Derivation
---

Implicit instances for some typeclass traits can be derived automatically. Example:
```scala
enum Tree[T] derives Eq, Ordering, Pickling {
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)
}
```
The derives clause automatically generates typeclass instances for
`Eq`, `Ordering`, and `Pickling` in the companion object `Tree`:
```scala
impl [T: Eq] of Eq[Tree[T]] = Eq.derived
impl [T: Ordering] of Ordering[Tree[T]] = Ordering.derived
impl [T: Pickling] of Pickling[Tree[T]] = Pickling.derived
```

**Note**: This page uses the new syntax proposed for implicits that is explored in #5448. This is not yet an endorsement of that syntax, but rather a way to experiment with it.

### Deriving Types

Besides for `enums`, typeclasses can also be derived for other sets of classes and objects that form an algebraic data type. These are:

 - individual case classes or case objects
 - sealed classes or traits that have only case classes and case objects as children.

 Examples:

 ```scala
case class Labelled[T](x: T, label: String) derives Eq, Show

sealed trait Option[T] derives Eq
case class Some[T] extends Option[T]
case object None extends Option[Nothing]
```

The generated typeclass instances are placed in the companion objects `Labelled` and `Option`, respectively.

### Derivable Traits

A trait can appear in a `derives` clause as long as

 - it has a single type parameter,
 - its companion object defines a method named `derived`.

These two conditions ensure that the synthesized derived instances for the trait are well-formed. The type and implementation of a `derived` method are arbitrary, but typically it has a definition like this:
```
  def derived[T] with (ev: Shaped[T, S]) = ...
```
That is, the `derived` method takes an implicit parameter of type `Shaped` that determines the _shape_ `S` of the deriving type `T` and it computes the typeclass implementation according to that shape. Implicit `Shaped` instances are generated automatically for all types that have a `derives` clause.

This is all a user of typeclass derivation has to know. The rest of this page contains information needed to be able to write a typeclass that can be used in a `derives` clause. In particular, it details the means provided for the implementation of data generic `derived` methods.


### The Shape Type

For every class with a `derives` clause, the compiler generates a type named `Shape` in the companion object of that class. For instance, here is the generated `Shape` type for the `Tree` enum:
```scala
type Shape[T] = Cases[
  Case[Branch[T], (Tree[T], Tree[T])],
  Case[Leaf[T], T *: Unit]
]
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

Here is the `Shape` type for `Labelled`:
```scala
type Shape[T] = Case[Labelled[T], (T, String)]
```
And here is the one for `Option`:
```scala
type Shape[T] = Cases[
  Case[Some[T], T * Unit],
  Case [None.type, Unit]
]
```
Note that an empty element tuple is represented as type `Unit`. A single-element tuple
is represented as `T *: Unit` since there is no direct syntax for such tuples: `(T)` is just `T` in parentheses, not a tuple.

The `Shape` type generation is suppressed if the companion object already contains a type member named `Shape`.

### The Shaped TypeClass

For every class `C[T_1,...,T_n]` with a `derives` clause, the compiler also generates a type class instance like this:
```scala
impl [T_1, ..., T_n] of Shaped[C[T_1,...,T_n], Shape[T_1,...,T_n]] ...
```
This instance is generated together with the `Shape` type in the companion object of the class.
For instance, the definition
```scala
enum Result[+T, +E] derives Logging {
  case class Ok[T](result: T)
  case class Err[E](err: E)
}
```
would produce the following members:
```scala
object Result {
  import scala.compiletime.Shape._

  type Shape[T, E] = Cases[(
    Case[Ok[T], T *: Unit],
    Case[Err[E], E *: Unit]
  )]

  impl [T, E] of Shaped[Result[T, E], Shape[T, E]] = ...
}
```

The `Shaped` class is defined in package `scala.reflect`.

```scala
abstract class Shaped[T, S <: Shape] extends Reflected[T]
```
It is a subclass of class `scala.reflect.Reflected`, which defines two methods that map between a type `T` and a generic representation of `T`, which we call a `Mirror`:
```scala
abstract class Reflected[T] {

  /** The mirror corresponding to ADT instance `x` */
  def reflect(x: T): Mirror

  /** The ADT instance corresponding to given `mirror` */
  def reify(mirror: Mirror): T

  /** The companion object of the ADT */
  def common: ReflectedClass
}
```

The `reflect` method maps an instance value of the ADT `T` to its mirror whereas the `reify` method goes the other way. There's also a `common` method that returns a value of type `ReflectedClass` which contains information that is the same
for all instances of a class (right now, this consists of essentially just the names of the cases and their parameters).

### Mirrors

A mirror is a generic representation of an instance value of an ADT. `Mirror` objects have three components:

 - `reflected: ReflectedClass`: The representation of the ADT class
 - `ordinal: Int`: The ordinal number of the case among all cases of the ADT, starting from 0
 - `elems: Product`: The elements of the instance, represented as a `Product`.

 The `Mirror` class is defined in package `scala.reflect` as follows:

```scala
class Mirror(val reflected: ReflectedClass, val ordinal: Int, val elems: Product) {

  /** The `n`'th element of this generic case */
  def apply(n: Int): Any = elems.productElement(n)

  /** The name of the constructor of the case reflected by this mirror */
  def caseLabel: String = reflected.label(ordinal)(0)

  /** The label of the `n`'th element of the case reflected by this mirror */
  def elementLabel(n: Int) = reflected.label(ordinal)(n + 1)
}
```

### ReflectedClass

Here's the API of `scala.reflect.ReflectedClass`:

```scala
class ReflectedClass(val runtimeClass: Class[_], labelsStr: String) {

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
```

The class provides four overloaded methods to create mirrors. The first of these is invoked by the `reify` method that maps an ADT instance to its mirror. It simply passes the
instance itself (which is a `Product`) to the second parameter of the mirror. That operation does not involve any copying and is thus quite efficient. The second and third versions of `mirror` are typically invoked by typeclass methods that create instances from mirrors. An example would be an `unpickle` method that first creates an array of elements, then creates
a mirror over that array, and finally uses the `reify` method in `Reflected` to create the ADT instance. The fourth version of `mirror` is used to create mirrors of instances that do not have any elements.

### How to Write Generic Typeclasses

Based on the machinery developed so far it becomes possible to define type classes generically. This means that the `derived` method will compute a type class instance for any ADT that has a `Shaped` instance, recursively.
The implementation of these methods typically uses three new typelevel constructs in Dotty: inline methods, inline matches and implicit matches. As an example, here is one possible implementation of a generic `Eq` type class, with explanations. Let's assume `Eq` is defined by the following trait:
```scala
trait Eq[T] {
  def eql(x: T, y: T): Boolean
}
```
We need to implement a method `Eq.derived` that produces an instance of `Eq[T]` provided
there exists evidence of type `Shaped[T, S]` for some shape `S`. Here's a possible solution:
```scala
  inline def derived[T, S <: Shape] with (ev: Shaped[T, S]): Eq[T] = new Eq[T] {
    def eql(x: T, y: T): Boolean = {
      val mx = ev.reflect(x)                    // (1)
      val my = ev.reflect(y)                    // (2)
      inline erasedValue[S] match {
        case _: Cases[alts] =>
          mx.ordinal == my.ordinal &&           // (3)
          eqlCases[alts](mx, my, 0)             // [4]
        case _: Case[_, elems] =>
          eqlElems[elems](mx, my, 0)            // [5]
      }
    }
  }
```
The implementation of the inline method `derived` creates an instance of `Eq[T]` and implements its `eql` method. The right hand side of `eql` mixes compile-time and runtime elements. In the code above, runtime elements are marked with a number in parentheses, i.e
`(1)`, `(2)`, `(3)`. Compile-time calls that expand to runtime code are marked with a number in brackets, i.e. `[4]`, `[5]`. The implementation of `eql` consists of the following steps.

  1. Map the compared values `x` and `y` to their mirrors using the `reflect` method of the implicitly passed `Shaped` evidence `(1)`, `(2)`.
  2. Match at compile-time against the type `S`. Dotty does not have a construct for matching types directly, buy we can emulate it using an `inline` match over an `erasedValue`. Depending on the actual type `S`, the match will reduce at compile time to one of its two alternatives.
  3. If `S` is of the form `Cases[alts]` for some tuple `alts` of alternative types, the equality test consists of comparing the ordinal values of the two mirrors `(3)` and, if they are equal, comparing the elements of the case indicated by that ordinal value. That second step is performed by code that results from the compile-time expansion of the `eqlCases` call `[4]`.
  4. If `S` is of the form `Case[elems]` for some tuple `elems` for element types, the elements of the case are compared by code that results from the compile-time expansion of the `eqlElems` call `[5]`.

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
The inline method `eqlCases` takes as type arguments the alternatives of the ADT that remain to be tested. It takes as value arguments mirrors of the two instances `x` and `y` to be compared and an integer `n` that indicates the ordinal number of the case that is tested next. Its produces an expression that compares these two values.

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
`eqlElems` takes as arguments the two mirrors of the elements to compare and a compile-time index `n`, indicating the index of the next element to test. It is defined in terms of an another compile-time match, this time over the tuple type `Elems` of all element types that remain to be tested. If that type is
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
    case ev: Eq[T] =>
      ev.eql(x, y)                              // (15)
    case _ =>
      error("No `Eq` instance was found for $T")
  }
```
`tryEql` is an inline method that takes an element type `T` and two element values of that type as arguments. It is defined using an `inline match` that tries to find an implicit instance of `Eq[T]`. If an instance `ev` is found, it proceeds by comparing the arguments using `ev.eql`. On the other hand, if no instance is found
this signals a compilation error: the user tried a generic derivation of `Eq` for a class with an element type that does not support an `Eq` instance itself. The error is signalled by
calling the `error` method defined in `scala.compiletime`.

**Note:** At the moment our error diagnostics for meta programming does not support yet interpolated string arguments for the `scala.compiletime.error` method that is called in the second case above. As an alternative, one can simply leave off the second case, then a missing typeclass would result in a "failure to reduce match" error.

**Example:** Here is a slightly polished and compacted version of the code that's generated by inline expansion for the derived `Eq` instance of class `Tree`.

```scala
impl Eq_Tree_impl[T] with (elemEq: Eq[T]) of Eq[Tree[T]] {
  def eql(x: Tree[T], y: Tree[T]): Boolean = {
    val ev = implOf[Shaped[Tree[T], Tree.Shape[T]]]
    val mx = ev.reflect(x)
    val my = ev.reflect(y)
    mx.ordinal == my.ordinal && {
      if (mx.ordinal == 0) {
        derived$Eq.eql(mx(0).asInstanceOf[Tree[T]], my(0).asInstanceOf[Tree[T]]) &&
        derived$Eq.eql(mx(1).asInstanceOf[Tree[T]], my(1).asInstanceOf[Tree[T]])
      }
      else if (mx.ordinal == 1) {
        elemEq.eql(mx(0).asInstanceOf[T], my(0).asInstanceOf[T])
      }
      else throw new MatchError(mx.ordinal)
    }
  }
}
```

One important difference between this approach and Scala-2 typeclass derivation frameworks such as Shapeless or Magnolia is that no automatic attempt is made to generate typeclass instances of elements recursively using the generic derivation framework. There must be an implicit instance of `Eq[T]` (which can of course be produced in turn using `Eq.derived`), or the compilation will fail. The advantage of this more restrictive approach to typeclass derivation is that it avoids uncontrolled transitive typeclass derivation by design. This keeps code sizes smaller, compile times lower, and is generally more predictable.

### Derived Instances Elsewhere

Sometimes one would like to derive a typeclass instance for an ADT after the ADT is defined, without being able to change the code of the ADT itself.
To do this, simply define an instance with the `derived` method of the typeclass as right hand side. E.g, to implement `Ordering` for `Option`, define:
```scala
impl [T: Ordering] of Ordering[Option[T]] = Ordering.derived
```
Usually, the `Ordering.derived` clause has an implicit parameter of type `Shaped[Option[T], Option.Shape[T]]`. Since the `Option` trait has a `derives` clause, the necessary implicit instance is already present in the companion object of `Option`. If the ADT in question does not have a `derives` clause, an implicit `Shaped` instance would still be synthesized by the compiler at the point where `derived` is called. This is similar to the situation with type tags or class tags: If no implicit instance is found, the compiler will synthesize one.

### Syntax

```
Template          ::=  InheritClauses [TemplateBody]
EnumDef           ::=  id ClassConstr InheritClauses EnumBody
InheritClauses    ::=  [‘extends’ ConstrApps] [‘derives’ QualId {‘,’ QualId}]
ConstrApps        ::=  ConstrApp {‘with’ ConstrApp}
                    |  ConstrApp {‘,’ ConstrApp}
```

### Discussion

The typeclass derivation framework is quite small and low-level. There are essentially two pieces of infrastructure that are generated by the compiler

 - The `Shape` type representing the shape of an ADT
 - A way to map between ADT instances and generic mirrors

Generic mirrors make use of the already existing `Product` infrastructure for case classes, which means they are efficient and their generation requires not much code.

Generic mirrors can be so simple because, just like `Product`s, they are weakly typed. On the other hand, this means that code for generic typeclasses has to ensure that type exploration and value selection proceed in lockstep and it has to assert this conformance in some places using casts. If generic typeclasses are correctly written these casts will never fail.

It could make sense to explore a higher-level framework that encapsulates all casts in the framework. This could give more guidance to the typeclass implementer. It also seems quite possible to put such a framework on top of the lower-level mechanisms presented here.