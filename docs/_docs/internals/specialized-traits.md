# Specialized Traits

Specialization is one of the few remaining desirable features from Scala 2 that's are as yet missing in Scala 3. We could try to port the Scala 2 scheme, which would be non-trivial since the implementation is quite complex. But that scheme is problematic enough to suggest that we also look for alternatives. A possible alternative is described here. It is meant to complement the [proposal on inline traits](https://github.com/lampepfl/dotty/issues/15532). That proposal also contains a more detailed critique of Scala 2 specialization.

The main problem of Scala-2 specialization is code bloat. We have to pro-actively generate up to 11 copies of functions and classes when they have a specialized type parameter, and this grows exponentially with the number of such type parameters. Miniboxing tries to reduce the number under the exponent from ~10 to 3 or 4, but it has problems dealing with arrays.

Languages like C++, Rust, Go, D, or Zig avoid the proactive generation of all possible specializations by monomorphizing the whole program. This means we only need to generate a specialized version of a function or class if it is actually used in the program. On the other hand, a global monomorphization can lead itself to code bloat and long compile times. It is also a problematic choice for binary APIs.

This note discusses a different scheme to get specialization for Scala 3, which is somewhat between Scala 2's selective specialization and full monomorphization. As in Scala 2, specialized type parameters are tagged explicitly (but not with an annotation). But as for monomorphization, specializations are only generated if a specialized type is referenced in the program. To make this work efficiently, we need a way to transport information about possible specialization types through generic code (full monomorphization does not need that since it eliminates all generic code).

We do that using a type class `Specialized` that is typically used as a context bound on a type parameter of some class. It indicates that we want to create specialized versions of that class where the type parameter is instantiated to the type argument. The specialized versions offer optimization opportunities compared to the generic class.

## Example

As a first example, consider a `Vec` trait for vectors over a numeric type.
```scala
import scala.math.Numeric

inline trait Vec[T: {Specialized, Numeric}](elems: Array[T]):

  def length = elems.length

  def apply(i: Int): T = elems(i)

  def scalarProduct(other: Vec[T]): T =
    require(this.length == other.length)
    var result = num.fromInt(0)
    for i <- 0 until length do
      result = num.plus(result, num.times(this(i), other(i)))
    result

object Vec:
  inline def apply[T: Specialized](elems: Array[T]) = new Vec[T](elems) {}
end Vec
```
The idea is that we want to specialize vectors on the type parameter `T` in order to get important efficiency gains, including the following:

 - Use an array `arr` specialized to the actual element instead of a fully generic array that has to be accessed via reflection
 - Avoid boxing for internal values like `result`
 - Avoid boxing in the API for values like the result of `scalarProduct`
 - Specialize on the concrete `Numeric` class instance for `T`, so that calls to `num`'s methods have static targets and can be inlined.

## Terminology and Restrictions

A _specialized trait_ is an inline trait that has at least one `Specialized` context bound.

A specialized context bound (or its expansion to a context parameter) is only allowed for
type parameters of inline methods and inline traits. Regular methods or traits or classes
cannot take `Specialized[T]` parameters. Hence, the only way to create a specialized trait is using an anonymous class instance, like in the `Vec.apply` method above. What's more,
we require that each such anonymous class instance

 - can extend only a single specialized trait,
 - cannot mix in further classes or traits, and
 - cannot contain member definitions.

So each such class instance is of the form `new A[Ts](ps1)...(psN) {}` where
`A` is a specialized trait and the type parameters `Ts` and term parameters `ps1, ,,, psN` which can also be absent.

The restrictions ensure that each time we create an instance of a specialized trait we know statically the classes of all `Specialized` type arguments. This enables us to implement the following expansion scheme:


## Expansion of Specialized Traits

A type instance of a specialized trait such as Vec[Tp] has a special erasure, which depends on the specializing supertype of `Tp`.

**Definition**: A _simple class type_ is a reference to a static class that does not have type parameters. References to traits and references containing non-static prefixes or refinements are excluded.

**Definition**: A top class is one of `Any`, `AnyVal`, or `Object`.

**Definition**: The _specializing supertype_ `SpecType(Tp)` of a type `Tp` is the smallest simple class type `C` such that

 - `Tp` is a subtype of `C`
 - The superclass of `C` is a top class, or `C` itself is a top class.

The _erasure_ of `Vec[Tp]` where `SpecType(Tp) = C` is:

 - If `C` is one of the top classes `Any` or `AnyRef` or `AnyVal`, the usual erased trait `Vec`.
 - If `C` is some other class, a new specialized instance trait with a name of the form `Vec$sp$TN`,
   where `$sp$` is a fixed specialization marker and `TN` is an encoding of the fully qualified name of `C`.

If there is more than one specialized type parameter, the specialized instance trait will reflect in its name all specializing supertypes of such type parameters in sequence.

An anonymous class instance creation like `new Vec[T](elems) {}` expands to
an instance creation `new Vec$impl$TN(elems)` of a new _specialized instance class_
named `Vec$impl$TN`. Here, `Vec$sp$TN` is the erasure of `Vec[T]` and the class name derives from that trait name by replacing `$sp` with `$impl$`.


The specialized instance traits are created on demand the first time they are mentioned in a type. For example, here is the definition of the specialized instance `Vec$sp$Int` for `Vec[Int]`:

```scala
trait Vec$sp$Int extends Vec[Int]:
  def length: Int
  def apply(x: Int): Int
  def scalarProduct(other: Vec[T]): Int
```

In general a specialized instance trait that specializes an inline trait `A[T]` with a specialization type `S`:

 - drops all specialized trait parameters of `A`,
 - adds `A[S]` as first parent trait,
 - _also_ adds all parents of `A` in their specialized forms,
 - contains all specialized declarations of `A`.

A specialized instance class for an inline trait `A` at specialized argument `S`

 - repeats the value parameters of trait `A`,
 - extends `A[S]`.

For example, here is the specialized instance class for `Vec` at `Int`:

```scala
class Vec$impl$Int(elems: Array[T]) extends Vec[Int]
```

After inlining `Vec[Int]` the expanded class looks like this:
```scala
class Vec(elems: Array[Int])(using Numeric[Int]):

  def length: Int = elems.length
  def apply(i: Int): Int = elems(i)

  def scalarProduct(other: Vec[Int]): Int =
    require(this.length == other.length)
    var result = num.fromInt(0)
    for i <- 0 until length do
      result = num.plus(result, num.times(this(i), other(i)))
    result
```

More examples of expansions are shown in the case study below.

## Caching of Specialized Traits and Classes

To avoid redundant repeated code generation of the same traits and classes, specialized instance traits and classes are cached. The compiler will put their tasty and classfile artifacts in a special directory
on the class path. Each artifact will contain in an annotation a hash of the  contents of the trait from which the instance was derived. Before creating a new specialized instance, the compiler will consult this directory to see whether an instance with the given name exists and whether its hash matches. In that case, the artifacts can be re-used.

## The `Specialized` Type Class

The `Specialized` Type Class is erased at runtime. Instances
of `Specialized[T]` are created automatically for types that do not contain type variables.

## A Larger Case Study

As an example of a hierarchy of specialized traits, consider the following small group of specialized collection traits:

```scala
inline trait Iterator[T: Specialized]:
  def hasNext: Boolean
  def next(): T

inline trait ArrayIterator[T: Specialized](elems: Array[T]) extends Iterator[T]:
  private var current = 0
  def hasNext: Boolean = current < elems.length
  def next(): T = try elems(current) finally current += 1

inline trait Iterable[T: Specialized]:
  def iterator: Iterator[T]
  def forall(f: T => Unit): Unit =
    val it = iterator
    while it.hasNext do f(it.next())

inline trait Seq[T: Specialized](elems: Array[T]) extends Iterable[T]:
  def length: Int           = elems.length
  def apply(i: Int): T      = elems(i)
  def iterator: Iterator[T] = new ArrayIterator[T](elems) {}
```

This generates the following instance traits:

```scala
trait Iterator$sp$Int extends Iterator[Int]:
  def hasNext: Boolean
  def next(): Int

trait ArrayIterator$sp$Int extends ArrayIterator[Int], Iterator[Int]

trait Iterable$sp$Int extends Iterable[Int]:
  def iterator: Iterator$sp$Int
  def forall(f: Int => Unit): Unit

trait Seq$sp$Int extends Seq[Int], Iterable[Int]:
  def length: Int
  def apply(i: Int): Int
```
Note that these traits repeat the parent types of their corresponding inline traits, for instance `ArrayIterator$sp$Int` extends `ArrayIterator[Int]` as well as its parent `Iterator[Int]`. After erasure, the definition of
`ArrayIterator$sp$Int` becomes
```scala
trait ArrayIterator$sp$Int extends ArrayIterator, Iterator$sp$Int
```
Hence, the erased `trait ArrayIterator$sp$Int` extends the general `ArrayIterator` trait as well as the specialized `Iterator$sp$Int` parent trait, which is what we want.

The specialized implementation classes for `ArrayIterator` and `Seq` are as follows:
```scala
class ArrayIterator$impl$Int(elems: Array[Int]) extends ArrayIterator$sp$Int:
  private var current = 0
  override def hasNext: Boolean =
    current < elems.length
  override def next(): Int =
    try elems(current) finally current += 1

class Seq$impl$Int(elems: Array[Int]) extends Seq$sp$Int:
  override def iterator: Iterator$sp$Int = new ArrayIterator$impl$Int(elems)

  override def forall(f: Int => Unit): Unit =
    val it = iterator
    while it.hasNext do f(it.next())
  override def length: Int = elems.length
  override def apply(i: Int): Int = elems(i)
```
These implementation classes are type correct as long as we inject the knowledge that a specialization trait
like `Seq$sp$Int` is equal to its parameterized version `Seq[Int]`. This equality holds once types are erased.
Before that we either have to assume it, or insert some casts, as shown in the test file
`tests/pos/specialized-traits-strawman.scala`.

After erasure, the implementation traits and classes look like this:

```scala
  trait Iterator$sp$Int extends Iterator:
    def hasNext: Boolean
    def next(): Int

  trait ArrayIterator$sp$Int extends ArrayIterator, Iterator$sp$Int

  trait Iterable$sp$Int extends Iterable:
    def iterator: Iterator$sp$Int
    def forall(f: Function1): Unit

  trait Seq$sp$Int extends Seq, Iterable$sp$Int:
    def length: Int
    def apply(i: Int): Int

  class ArrayIterator$impl$Int(elems: Int[]) extends ArrayIterator$sp$Int:
    private var current = 0
    override def hasNext: Boolean =
      current < elems.length
    override def next(): Int =
      try elems(current) finally current += 1

    /* Bridges:
    override def next(): Object = Int.box(next())
    */
  end ArrayIterator$impl$Int

  class Seq$impl$Int(elems: Int[]) extends Seq$sp$Int:
    override def iterator: Iterator$sp$Int =
      new ArrayIterator$impl$Int(elems)
    override def forall(f: Function1): Unit =
      val it = iterator
      while it.hasNext do f.apply$mcVI$sp(it.next())
    override def length: Int = elems.length
    override def apply(i: Int): Int = elems(i)

    /* Bridges:
    override def iterator: Iterator = iterator
    override def apply(i: Int): Object = Int.box(apply(i))
    */
  end Seq$impl$Int
```
Here, `f.apply$mcVI$sp` is the specialized apply method of `Function1` at type `Int => Unit`.
This method is generated by Scala 2's function specialization which is also adopted by Scala 3.

The example shows that indeed all code is properly specialized with no need for box or unbox operations.


## Conclusion

The described scheme is surprisingly simple. All the heavy lifting is done by inline traits. Adding specialization on top requires little more than arranging for a cache of specialized instances.

The scheme requires explicit monomorphization through inline methods and inline traits. One point to investigate further is how convenient and expressive code adhering to that restriction can be. If we take specialized collections as
an example, if we want the result of `map` to be specialized, we have to define `map` as an inline method:
```scala
package collection.immutable.faster
inline trait Vector[+A: Specialized](elems: A*):
  ...
  inline def map[B: Specialized](f: A => B): Vector[B] =
    new Vector[B](elems.map(f))
```
There's precedent for this in Kotlin where the majority of higher-order collection methods are declared inline, in this case  in order to allow specialization for suspendability. So the restriction does not look like a blocker.

Some flexibility could be gained if we allowed method overloading between specialized inline methods and normal methods with matching type signatures. For instance, the `Vector` implementation above seriously restricts `map` by requiring that its `B` type parameter is also `Specialized`. Thus `map` cannot be used to map a specialized collection to another collection if the result element type is not ground. But we could alleviate the problem by allowing a second, overloaded `map` operation like this:
```scala
  def map[B](f: A => B): collection.immutable.Vector[B] =
    new collection.immutable.Vector[B](elems.map(f))
```
The second implementation of `map` will return an unspecialized vector if
the new element type is not statically known. If overloads like this were allowed, they could be resolved by picking the specialized inline version if
a `Specialized` instance can be synthesized for the actual type argument, and picking the unspecialized version otherwise.







