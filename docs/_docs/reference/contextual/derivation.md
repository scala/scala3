---
layout: doc-page
title: "Type Class Derivation"
nightlyOf: https://docs.scala-lang.org/scala3/reference/contextual/derivation.html
---

Type class derivation is a way to automatically generate given instances for type classes which satisfy some simple
conditions. A type class in this sense is any trait or class with a single type parameter determining the type being operated
on, and the special case `CanEqual`. Common examples are `Eq`, `Ordering`, or `Show`. For example, given the following `Tree` algebraic data type
(ADT):

```scala
enum Tree[T] derives Eq, Ordering, Show:
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)
```

The `derives` clause generates the following given instances for the `Eq`, `Ordering` and `Show` type classes in the
companion object of `Tree`:

```scala
given [T: Eq]       => Eq[Tree[T]]       = Eq.derived
given [T: Ordering] => Ordering[Tree[T]] = Ordering.derived
given [T: Show]     => Show[Tree[T]]     = Show.derived
```

We say that `Tree` is the _deriving type_ and that the `Eq`, `Ordering` and `Show` instances are _derived instances_.

**Note:** `derived` can be used manually, this is useful when you do not have control over the definition. For example we can implement `Ordering` for `Option`s like so:

```scala
given [T: Ordering] => Ordering[Option[T]] = Ordering.derived
```

It is discouraged to directly refer to the `derived` member if you can use a `derives` clause instead.

All data types can have a `derives` clause. This document focuses primarily on data types which also have a given instance
of the `Mirror` type class available.

## Exact mechanism
In the following, when type arguments are enumerated and the first index evaluates to a larger value than the last, then there are actually no arguments, for example: `A[T_2, ..., T_1]` means `A`.

For a class/trait/object/enum `DerivingType[T_1, ..., T_N] derives TC`, a derived instance is created in `DerivingType`'s companion object (or `DerivingType` itself if it is an object).

The general "shape" of the derived instance is as follows:
```scala
given [...] => (...) => TC[ ... DerivingType[...] ... ] = TC.derived
```
`TC.derived` should be an expression that conforms to the expected type on the left, potentially elaborated using term and/or type inference.

**Note:** `TC.derived` is a normal access, therefore if there are multiple definitions of `TC.derived`, overloading resolution applies.

What the derived instance precisely looks like depends on the specifics of `DerivingType` and `TC`, we first examine `TC`:

### `TC` takes 1 parameter `F`

Therefore `TC` is defined as `TC[F[A_1, ..., A_K]]` (`TC[F]` if `K == 0`) for some `F`.
There are two further cases depending on the kinds of arguments:

#### `F` and all arguments of `DerivingType` have kind `*`
**Note:** `K == 0` in this case.

The generated instance is then:
```scala
given [T_1: TC, ..., T_N: TC] => TC[DerivingType[T_1, ..., T_N]] = TC.derived
```

This is the most common case, and is the one that was highlighted in the introduction.

**Note:** The `[T_i: TC, ...]` introduces a `(using TC[T_i], ...)`, more information in [Context Bounds](./context-bounds.md).
This allows the `derived` member to access these evidences.

**Note:** If `N == 0` the above means:
```scala
given TC[DerivingType] = TC.derived
```
For example, the class
```scala
case class Point(x: Int, y: Int) derives Ordering
```
generates the instance
```scala
object Point:
  ...
  given Ordering[Point] = Ordering.derived
```


#### `F` and `DerivingType` have parameters of matching kind on the right
This section covers cases where you can pair arguments of `F` and `DerivingType` starting from the right such that they have the same kinds pairwise, and all arguments of `F` or `DerivingType` (or both) are used up.
`F` must also have at least one parameter.

The general shape will then be:
```scala
given [...] => TC[ [...] =>> DerivingType[...] ] = TC.derived
```
Where of course `TC` and `DerivingType` are applied to types of the correct kind.

To make this work, we split it into 3 cases:

If `F` and `DerivingType` take the same number of arguments (`N == K`):
```scala
given TC[DerivingType] = TC.derived
// simplified form of:
given TC[ [A_1, ..., A_K] =>> DerivingType[A_1, ..., A_K] ] = TC.derived
```
If `DerivingType` takes fewer arguments than `F` (`N < K`), we use only the rightmost parameters from the type lambda:
```scala
given TC[ [A_1, ..., A_K] =>> DerivingType[A_(K-N+1), ..., A_K] ] = TC.derived

// if DerivingType takes no arguments (N == 0), the above simplifies to:
given TC[ [A_1, ..., A_K] =>> DerivingType ] = TC.derived
```

If `F` takes fewer arguments than `DerivingType` (`K < N`), we fill in the remaining leftmost slots with type parameters of the given:
```scala
given [T_1, ... T_(N-K)] => TC[[A_1, ..., A_K] =>> DerivingType[T_1, ... T_(N-K), A_1, ..., A_K]] = TC.derived
```

### `TC` is the `CanEqual` type class

We have therefore: `DerivingType[T_1, ..., T_N] derives CanEqual`.

Let `U_1`, ..., `U_M` be the parameters of `DerivingType` of kind `*`.
(These are a subset of the `T_i`s)

The generated instance is then:
```scala
given [T_1L, T_1R, ..., T_NL, T_NR]                            // every parameter of DerivingType twice
      (using CanEqual[U_1L, U_1R], ..., CanEqual[U_ML, U_MR]): // only parameters of DerivingType with kind *
        CanEqual[DerivingType[T_1L, ..., T_NL], DerivingType[T_1R, ..., T_NR]] = // again, every parameter
          CanEqual.derived
```

The bounds of `T_i`s are handled correctly, for example: `T_2 <: T_1` becomes `T_2L <: T_1L`.

For example, the class
```scala
class MyClass[A, G[_]](a: A, b: G[B]) derives CanEqual
```
generates the following given instance:
```scala
object MyClass:
  ...
  given [A_L, A_R, G_L[_], G_R[_]] => CanEqual[A_L, A_R] => CanEqual[MyClass[A_L, G_L], MyClass[A_R, G_R]] = CanEqual.derived
```

### `TC` is not valid for automatic derivation

Throw an error.

The exact error depends on which of the above conditions failed.
As an example, if `TC` takes more than 1 parameter and is not `CanEqual`, the error is `DerivingType cannot be unified with the type argument of TC`.

All data types can have a `derives` clause. The rest of this document focuses primarily on data types which also have a given instance
of the `Mirror` type class available.

## `Mirror`

`scala.deriving.Mirror` type class instances provide information at the type level about the components and labelling of the type.
They also provide minimal term-level infrastructure to allow higher-level libraries to provide comprehensive
derivation support.

Instances of the `Mirror` type class are generated automatically by the compiler
unconditionally for:
- enums and enum cases,
- case objects.

Instances for `Mirror` are also generated conditionally for:
- case classes where the constructor is visible at the callsite (always true if the companion is not a case object)
- sealed classes and sealed traits where:
  - there exists at least one child case,
  - each child case is reachable from the parent's definition,
  - if the sealed trait/class has no companion, then each child case is reachable from the callsite through the prefix of the type being mirrored,
  - and where the compiler can generate a `Mirror` type class instance for each child case.


The `scala.deriving.Mirror` type class definition is as follows:

```scala
sealed trait Mirror:

  /** the type being mirrored */
  type MirroredType

  /** the type of the elements of the mirrored type */
  type MirroredElemTypes

  /** The mirrored *-type */
  type MirroredMonoType

  /** The name of the type */
  type MirroredLabel <: String

  /** The names of the elements of the type */
  type MirroredElemLabels <: Tuple

object Mirror:

  /** The Mirror for a product type */
  trait Product extends Mirror:

    /** Create a new instance of type `T` with elements
     *  taken from product `p`.
     */
    def fromProduct(p: scala.Product): MirroredMonoType

  trait Sum extends Mirror:

    /** The ordinal number of the case class of `x`.
     *  For enums, `ordinal(x) == x.ordinal`
     */
    def ordinal(x: MirroredMonoType): Int

end Mirror
```

Product types (i.e. case classes and objects, and enum cases) have mirrors which are subtypes of `Mirror.Product`. Sum
types (i.e. sealed class or traits with product children, and enums) have mirrors which are subtypes of `Mirror.Sum`.

For the `Tree` ADT from above the following `Mirror` instances will be automatically provided by the compiler,

```scala
// Mirror for Tree
new Mirror.Sum:
  type MirroredType = Tree
  type MirroredElemTypes[T] = (Branch[T], Leaf[T])
  type MirroredMonoType = Tree[_]
  type MirroredLabel = "Tree"
  type MirroredElemLabels = ("Branch", "Leaf")

  def ordinal(x: MirroredMonoType): Int = x match
    case _: Branch[_] => 0
    case _: Leaf[_] => 1

// Mirror for Branch
new Mirror.Product:
  type MirroredType = Branch
  type MirroredElemTypes[T] = (Tree[T], Tree[T])
  type MirroredMonoType = Branch[_]
  type MirroredLabel = "Branch"
  type MirroredElemLabels = ("left", "right")

  def fromProduct(p: Product): MirroredMonoType =
    new Branch(...)

// Mirror for Leaf
new Mirror.Product:
  type MirroredType = Leaf
  type MirroredElemTypes[T] = Tuple1[T]
  type MirroredMonoType = Leaf[_]
  type MirroredLabel = "Leaf"
  type MirroredElemLabels = Tuple1["elem"]

  def fromProduct(p: Product): MirroredMonoType =
    new Leaf(...)
```

If a Mirror cannot be generated automatically for a given type, an error will appear explaining why it is neither a supported
sum type nor a product type. For example, if `A` is a trait that is not sealed,

```
No given instance of type deriving.Mirror.Of[A] was found for parameter x of method summon in object Predef. Failed to synthesize an instance of type deriving.Mirror.Of[A]:
     * trait A is not a generic product because it is not a case class
     * trait A is not a generic sum because it is not a sealed trait
```


Note the following properties of `Mirror` types,

+ Properties are encoded using types rather than terms. This means that they have no runtime footprint unless used and
  also that they are a compile-time feature for use with Scala 3's metaprogramming facilities.
+ There is no restriction against the mirrored type being a local or inner class.
+ The kinds of `MirroredType` and `MirroredElemTypes` match the kind of the data type the mirror is an instance for.
  This allows `Mirror`s to support ADTs of all kinds.
+ There is no distinct representation type for sums or products (ie. there is no `HList` or `Coproduct` type as in
  Scala 2 versions of Shapeless). Instead the collection of child types of a data type is represented by an ordinary,
  possibly parameterized, tuple type. Scala 3's metaprogramming facilities can be used to work with these tuple types
  as-is, and higher-level libraries can be built on top of them.
+ For both product and sum types, the elements of `MirroredElemTypes` are arranged in definition order (i.e. `Branch[T]`
  precedes `Leaf[T]` in `MirroredElemTypes` for `Tree` because `Branch` is defined before `Leaf` in the source file).
  This means that `Mirror.Sum` differs in this respect from Shapeless's generic representation for ADTs in Scala 2,
  where the constructors are ordered alphabetically by name.
+ The methods `ordinal` and `fromProduct` are defined in terms of `MirroredMonoType` which is the type of kind-`*`
  which is obtained from `MirroredType` by wildcarding its type parameters.

## Implementing `derived` with `Mirror`

As seen before, the signature and implementation of a `derived` method for a type class `TC[_]` are arbitrary, but we expect it to typically be of the following form:

```scala
import scala.deriving.Mirror

inline def derived[T](using Mirror.Of[T]): TC[T] = ...
```

That is, the `derived` method takes a context parameter of (some subtype of) type `Mirror` which defines the shape of
the deriving type `T`, and computes the type class implementation according to that shape. This is all that the
provider of an ADT with a `derives` clause has to know about the derivation of a type class instance.

Note that `derived` methods may have context `Mirror` parameters indirectly (e.g. by having a context argument which in turn
has a context `Mirror` parameter, or not at all (e.g. they might use some completely different user-provided mechanism, for
instance using Scala 3 macros or runtime reflection). We expect that (direct or indirect) `Mirror` based implementations
will be the most common and that is what this document emphasises.

Type class authors will most likely use higher-level derivation or generic programming libraries to implement
`derived` methods. An example of how a `derived` method might be implemented using _only_ the low-level facilities
described above and Scala 3's general metaprogramming features is provided below. It is not anticipated that type class
authors would normally implement a `derived` method in this way, however this walkthrough can be taken as a guide for
authors of the higher-level derivation libraries that we expect typical type class authors will use (for a fully
worked out example of such a library, see [Shapeless 3](https://github.com/milessabin/shapeless/tree/shapeless-3)).

## How to write a type class `derived` method using low-level mechanisms

The low-level technique we will use to implement a type class `derived` method in this example exploits three new type-level constructs in Scala 3: inline methods, inline matches, and implicit searches via  `summonInline` or `summonFrom`.
Given this definition of the `Eq` type class,

```scala
trait Eq[T]:
  def eqv(x: T, y: T): Boolean
```

we need to implement a method `Eq.derived` on the companion object of `Eq` that produces a given instance for `Eq[T]` given
a `Mirror[T]`.
Here is a possible implementation,

```scala
import scala.deriving.Mirror

inline def derived[T](using m: Mirror.Of[T]): Eq[T] =
  lazy val elemInstances = summonInstances[T, m.MirroredElemTypes] // (1)
  inline m match                                                   // (2)
    case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
    case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
```

Note that `derived` is defined as an `inline def`.
This means that the method will be inlined at all call sites (for instance the compiler-generated instance definitions in the companion objects of ADTs which have a `deriving Eq` clause).

> Inlining of complex code is potentially expensive if overused (meaning slower compile times) so we should be careful to limit how many times `derived` is called for the same type.
> For example, when computing an instance for a sum type, it may be necessary to call `derived` recursively to compute an instance for each one of its child cases.
> That child case may in turn be a product type, that declares a field referring back to the parent sum type.
> To compute the instance for this field, we should not call `derived` recursively, but instead summon from the context.
> Typically, the found given instance will be the root given instance that initially called `derived`.

The body of `derived` (1) first materializes the `Eq` instances for all the child types of type the instance is being derived for.
This is either all the branches of a sum type or all the fields of a product type.
The implementation of `summonInstances` is `inline` and uses Scala 3's `summonInline` construct to collect the instances as a `List`,

```scala
inline def summonInstances[T, Elems <: Tuple]: List[Eq[?]] =
  inline erasedValue[Elems] match
    case _: (elem *: elems) => deriveOrSummon[T, elem] :: summonInstances[T, elems]
    case _: EmptyTuple => Nil

inline def deriveOrSummon[T, Elem]: Eq[Elem] =
  inline erasedValue[Elem] match
    case _: T => deriveRec[T, Elem]
    case _    => summonInline[Eq[Elem]]

inline def deriveRec[T, Elem]: Eq[Elem] =
  inline erasedValue[T] match
    case _: Elem => error("infinite recursive derivation")
    case _       => Eq.derived[Elem](using summonInline[Mirror.Of[Elem]]) // recursive derivation
```

with the instances for children in hand the `derived` method uses an `inline match` to dispatch to methods which can
construct instances for either sums or products (2).
Note that because `derived` is `inline` the match will be resolved at compile-time and only the right-hand side of the matching case will be inlined into the generated code with types refined as revealed by the match.

In the sum case, `eqSum`, we use the runtime `ordinal` values of the arguments to `eqv` to first check if the two values are of the same subtype of the ADT (3) and then, if they are, to further test for equality based on the `Eq` instance for the appropriate ADT subtype using the auxiliary method `check` (4).

```scala
import scala.deriving.Mirror

def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[?]]): Eq[T] =
  new Eq[T]:
    def eqv(x: T, y: T): Boolean =
      val ordx = s.ordinal(x)                            // (3)
      (s.ordinal(y) == ordx) && check(x, y, elems(ordx)) // (4)
```

In the product case, `eqProduct`, we test the runtime values of the arguments to `eqv` for equality as products based on the `Eq` instances for the fields of the data type (5),

```scala
import scala.deriving.Mirror

def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[?]]): Eq[T] =
  new Eq[T]:
    def eqv(x: T, y: T): Boolean =
      iterable(x).lazyZip(iterable(y)).lazyZip(elems).forall(check)
```

Both `eqSum` and `eqProduct` have a by-name parameter `elems`, because the argument passed is the reference to the lazy `elemInstances` value.

Pulling this all together we have the following complete implementation,

```scala
import scala.collection.AbstractIterable
import scala.compiletime.{erasedValue, error, summonInline}
import scala.deriving.*

inline def summonInstances[T, Elems <: Tuple]: List[Eq[?]] =
  inline erasedValue[Elems] match
    case _: (elem *: elems) => deriveOrSummon[T, elem] :: summonInstances[T, elems]
    case _: EmptyTuple => Nil

inline def deriveOrSummon[T, Elem]: Eq[Elem] =
  inline erasedValue[Elem] match
    case _: T => deriveRec[T, Elem]
    case _    => summonInline[Eq[Elem]]

inline def deriveRec[T, Elem]: Eq[Elem] =
  inline erasedValue[T] match
    case _: Elem => error("infinite recursive derivation")
    case _       => Eq.derived[Elem](using summonInline[Mirror.Of[Elem]]) // recursive derivation

trait Eq[T]:
  def eqv(x: T, y: T): Boolean

object Eq:
  given Eq[Int]:
    def eqv(x: Int, y: Int) = x == y

  def check(x: Any, y: Any, elem: Eq[?]): Boolean =
    elem.asInstanceOf[Eq[Any]].eqv(x, y)

  def iterable[T](p: T): Iterable[Any] = new AbstractIterable[Any]:
    def iterator: Iterator[Any] = p.asInstanceOf[Product].productIterator

  def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[?]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        val ordx = s.ordinal(x)
        (s.ordinal(y) == ordx) && check(x, y, elems(ordx))

  def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[?]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        iterable(x).lazyZip(iterable(y)).lazyZip(elems).forall(check)

  inline def derived[T](using m: Mirror.Of[T]): Eq[T] =
    lazy val elemInstances = summonInstances[T, m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
end Eq
```

we can test this relative to a simple ADT like so,

```scala
enum Lst[+T] derives Eq:
  case Cns(t: T, ts: Lst[T])
  case Nl

extension [T](t: T) def ::(ts: Lst[T]): Lst[T] = Lst.Cns(t, ts)

@main def test(): Unit =
  import Lst.*
  val eqoi = summon[Eq[Lst[Int]]]
  assert(eqoi.eqv(23 :: 47 :: Nl, 23 :: 47 :: Nl))
  assert(!eqoi.eqv(23 :: Nl, 7 :: Nl))
  assert(!eqoi.eqv(23 :: Nl, Nl))
```

In this case the code that is generated by the inline expansion for the derived `Eq` instance for `Lst` looks like the
following, after a little polishing,

```scala
given derived$Eq[T] => (eqT: Eq[T]) => Eq[Lst[T]] =
  eqSum(summon[Mirror.Of[Lst[T]]], {/* cached lazily */
    List(
      eqProduct(summon[Mirror.Of[Cns[T]]], {/* cached lazily */
        List(summon[Eq[T]], summon[Eq[Lst[T]]])
      }),
      eqProduct(summon[Mirror.Of[Nl.type]], {/* cached lazily */
        Nil
      })
    )
  })
```

The `lazy` modifier on `elemInstances` is necessary for preventing infinite recursion in the derived instance for recursive types such as `Lst`.

Alternative approaches can be taken to the way that `derived` methods can be defined. For example, more aggressively
inlined variants using Scala 3 macros, whilst being more involved for type class authors to write than the example
above, can produce code for type classes like `Eq` which eliminate all the abstraction artefacts (eg. the `Lists` of
child instances in the above) and generate code which is indistinguishable from what a programmer might write by hand.
As a third example, using a higher-level library such as Shapeless, the type class author could define an equivalent
`derived` method as,

```scala
given eqSum: [A] => (inst: => K0.CoproductInstances[Eq, A]) => Eq[A]:
  def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false)(
    [t] => (eqt: Eq[t], t0: t, t1: t) => eqt.eqv(t0, t1)
  )

given eqProduct: [A] => (inst: => K0.ProductInstances[Eq, A]) => Eq[A]:
  def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean)(
    [t] => (acc: Boolean, eqt: Eq[t], t0: t, t1: t) =>
      Complete(!eqt.eqv(t0, t1))(false)(true)
  )

inline def derived[A](using gen: K0.Generic[A]): Eq[A] =
  gen.derive(eqProduct, eqSum)
```

The framework described here enables all three of these approaches without mandating any of them.

For a brief discussion on how to use macros to write a type class `derived`
method, please read more at [How to write a type class `derived` method using macros](./derivation-macro.md).

## Syntax

```ebnf
Template          ::=  InheritClauses [TemplateBody]
EnumDef           ::=  id ClassConstr InheritClauses EnumBody
InheritClauses    ::=  [‘extends’ ConstrApps] [‘derives’ QualId {‘,’ QualId}]
ConstrApps        ::=  ConstrApp {‘with’ ConstrApp}
                    |  ConstrApp {‘,’ ConstrApp}
```

**Note:** To align `extends` clauses and `derives` clauses, Scala 3 also allows multiple
extended types to be separated by commas. So the following is now legal:

```scala
class A extends B, C { ... }
```

It is equivalent to the old form

```scala
class A extends B with C { ... }
```

## Discussion

This type class derivation framework is intentionally very small and low-level. There are essentially two pieces of
infrastructure in compiler-generated `Mirror` instances,

+ type members encoding properties of the mirrored types.
+ a minimal value-level mechanism for working generically with terms of the mirrored types.

The `Mirror` infrastructure can be seen as an extension of the existing `Product` infrastructure for case classes:
typically, `Mirror` types will be implemented by the ADTs companion object, hence the type members and the `ordinal` or
`fromProduct` methods will be members of that object. The primary motivation for this design decision, and the
decision to encode properties via types rather than terms was to keep the bytecode and runtime footprint of the
feature small enough to make it possible to provide `Mirror` instances _unconditionally_.

Whilst `Mirrors` encode properties precisely via type members, the value-level `ordinal` and `fromProduct` are
somewhat weakly typed (because they are defined in terms of `MirroredMonoType`) just like the members of `Product`.
This means that code for generic type classes has to ensure that type exploration and value selection proceed in
lockstep and it has to assert this conformance in some places using casts. If generic type classes are correctly
written these casts will never fail.

As mentioned, however, the compiler-provided mechanism is intentionally very low-level and it is anticipated that
higher-level type class derivation and generic programming libraries will build on this and Scala 3's other
metaprogramming facilities to hide these low-level details from type class authors and general users. Type class
derivation in the style of both Shapeless and Magnolia are possible (a prototype of Shapeless 3, which combines
aspects of both Shapeless 2 and Magnolia has been developed alongside this language feature) as is a more aggressively
inlined style, supported by Scala 3's new quote/splice macro and inlining facilities.
