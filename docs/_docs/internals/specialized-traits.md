# Specialized Traits
Specialized traits accompany inline traits as a new attempt to solve to the specialization problem in Scala 3, replacing the `@specialized` annotation from Scala 2.

As mentioned in the accompanying document on inline traits, inline traits have one shortcoming, namely that interfaces are not specialized. For example:

```scala
inline trait Foo[T](x: T):
    def foo: T = x

class Bar extends Foo[Int](42)

def f(b: Foo[Int]) = 37 + b.foo
 
@main def main =
    val x = Bar()
    f(x)
```

In this code the call to `b.foo` will refer to the version of `foo` typed `foo: T` which becomes `foo: Object` during erasure, because we accessed `foo` on
an object of declared type `Foo` (even though `b`'s actual runtime type is `Bar`). This will in turn call a bridge method 
which means the `foo: Int` method will be called, but unnecessary boxing and unboxing will be added:

```scala
inline trait Foo
    def foo#1(): Object

class Bar extends Foo[Int](42):
    override def foo#2(): Int = 42
    override def foo#3(): Object = Int.box(this.foo())

def f(b: Foo): Int = 37 + Int.unbox(b.foo#1()) // virtual call to foo#1 resolves to bridge method foo#3, which in turn calls actual method foo#2, adding boxing. 
```

We introduce the typeclass `Specialized` that can be used as a context bound on type parameters of inline traits and inline methods.

The `Specialized` context bound indicates that we want to create specialized versions of the inline trait where the type parameter is instantiated to the type argument, and replace (in erasure) uses of the inline trait with the corresponding specialized version so that member accesses pass through the specialized interface.

A `Specialized` context bound on an inline method allows it to call other specialized methods or instantiate specialized traits using the specialized type variable (any code may instantiate specialized traits with concrete types). This allows us to transport information about possible specialization types through multiple layers of generic inline methods (see Transportation of Specialized through Generic Code).

## Example

To give a flavour of what specialization does, consider a `Vec` trait for vectors over a numeric type:
```scala
import scala.math.Numeric

inline trait Vec[T: {Specialized, Numeric}](elems: Array[T]):
  private val num = summon[Numeric[T]]

  def length = elems.length

  def apply(i: Int): T = elems(i)

  def scalarProduct(other: Vec[T]): T =
    require(this.length == other.length)
    var result = num.fromInt(0)
    for i <- 0 until length do
      result = num.plus(result, num.times(this(i), other(i)))
    result

def printVector(v: Vec[Int]) = println(v)

object Vec:
  inline def apply[T: Specialized](elems: Array[T]) = new Vec[T](elems) {}
end Vec

val v = Vec[Int](Array(1, 2, 3, 4, 5))
```

Specialization will generate:
```scala
inline trait Vec$sp$Int extends Vec[Int]:
  def length: Int
  def apply(i: Int): Int
  def scalarProduct(other: Vec$sp$Int): Int

class Vec$impl$Int(elems: Array[Int])(using Numeric[Int]) extends Vec[Int](elems), Vec$sp$Int:
  private val Vec$$num = summon[Numeric[Int]]

  def length = elems.length

  def apply(i: Int): Int = elems(i)

  def scalarProduct(other: Vec$sp$Int): Int =
    require(this.length == other.length)
    var result = Vec$$num.fromInt(0)
    for i <- 0 until length do
      result = Vec$$num.plus(result, Vec$$num.times(this(i), other(i)))
    result

def printVector(v: Vec$sp$Int) = println(v)

object Vec:
  inline def apply[T: Specialized](elems: Array[T]) = new Vec[T](elems) {}
end Vec

val v = Vec$impl$Int(Array(1, 2, 3, 4, 5))
```

This provides a number of potential efficiency gains:
 - Avoid boxing in the API for values like the result of `scalarProduct` (as we will access the method on the specialized version).
 - Specialize on the concrete `Numeric` class instance for `T`, so that calls to `num`'s methods have static targets and can be inlined, and avoid boxing for internal values like `result` (if we use define and use a specialized version of Numeric).
 - Use an array `elems` specialized to the actual element instead of a fully generic array that has to be accessed via reflection. (The rest of the compiler already generates `int[]` when the source references `Array[Int]` so this comes for free through inline trait specialization).

## Creating Specialized Trait Instances
Creation of an object with specialized behaviour can occur in one of two ways:
 - Instantiating a `class` or using an `object` [1, 2] which extends the specialized trait, specializing its type parameters. E.g.:
```scala
inline trait Foo[T: Specialized](x: T):
  def foo: T = x

class Bar extends Foo[Int](10): // Type parameter does of course not need to be specified explicitly here (it can be inferred from the value type of 10)
    def myMethod = "Hello I am a method"

// Both Baz and myBar will have specialized versions of foo.
object Baz extends Foo(12)
val myBar = Bar()
```
 - Instantiation of an anonymous class instance directly from the Specialized trait: `new A[Ts](ps1)...(psN) {}`  where `A` is a specialized trait and the type parameters `Ts` and term parameters `ps1, ,,, psN` can also be absent. This has a special meaning, as it desugars to instantiating a _specialized instance class_ (`$impl$` class, see Expansion of Specialized Traits). This comes with the twin advantages that:
    - there is no need for the boilerplate of manually defining an object/class to extend the specialized trait
    - this specialized instance class is reused every time such an instance is created (there is no proliferation of anonymous classes).
  
    However, to facilitate this reuse, we must impose the following restrictions. Anonymous class instances acting as instances of Specialized traits:
    - can extend only a single specialized trait [0],
    - cannot mix in further classes or traits, and
    - cannot contain member definitions.
    
    Should these restrictions be undesirable, the user can always create their own named `object` or `class` extending from a specialized trait (i.e. the first case for creation of a specialized object just above), which does not induce these restrictions.

[0] Note that because the specialized traits take evidence parameters for the `Specialized` typeclass, `new Foo[Int] {}` where `Foo` extends some other specialized trait `Bar` desugars in the compiler to `new Bar[Int] with Foo[Int] {}`, which means we can't distinguish these two cases. Therefore we allow 
e.g. `new Bar[Int] with Foo[Int] {}` although there is no reason to use this in source code because it's exactly the same as writing `new Foo[Int] {}`.

Note that 'class extends specialized trait' and 'object extends specialized trait' are allowed. However, 'trait extends specialized trait' is not. This is due to the restriction on extending inline traits with parameters by ordinary traits, as discussed in `inline-traits.md`. 'inline trait extends specialized trait' is allowed as it is not subject to this restriction.

As explained in detail in _Specialized Traits Expansion Specification_, specialized instance traits and implementation classes have a special erasure. Classes/objects extending a specialized trait also have special erasure behaviour which adds both the specialized parents and ordinary parents:

```scala
class Bar extends Foo[Int](10) with Baz[Int](10)
class Bar extends Object, Foo(10), Bar(10), Foo$sp$Int, Bar$sp$Int
```
This is necessary so that Bar can be treated as a `Bar$sp$Int` or `Foo$sp$Int` (in general we may not have `Bar$sp$Int` extends `Foo$sp$Int` so we add both) and to pass arguments to `Foo` and `Bar`.

## Specialized Traits Expansion Specification
The following is a technical specification for the expansion of specialized traits.
 
### Expansion of Specialized Traits
**Definition**: A _specialized trait_ is an inline trait that has at least one `Specialized` context bound. A specialized context bound (or its expansion to a context parameter) is only allowed for
type parameters of inline methods and inline traits. Regular methods or traits or classes
cannot take `Specialized[T]` parameters. 

**Definition**: A _simple class type_ is a reference to a static class that does not have type parameters. References to traits and references containing non-static prefixes or refinements are excluded.

**Definition**: A _top class_ is one of `Any`, `AnyVal`, or `Object` (also `AnyRef` which is a synonym of `Object`).

**Definition**: The _specializing supertype_ `SpecType(Tp)` of a type `Tp` is defined as follows:
 - `SpecType(Tp) =` the smallest simple class type `C` such that:
    - `C` is a supertype of `Tp` 
    - The superclass of `C` is a top class, or `C` is itself a top class.
- `SpecType(Nothing) = Nothing`

> By smallest simple class type we mean in the sense of smallest set of values that the type contains, i.e. the closest to the leaves of the class hierarchy. This simply means we may have a choice between `Foo` and `Any`, with `Foo <:< Any` and we will select `Foo`.

**Definition** A _specialization_ `SpecTrait[T1, T2, T3, ...]` is a reference to a specialized trait with concrete type arguments.

The presence of a specialization `SpecTrait[T1, T2, T3, ...]` in the program implies the creation of a _specialized interface trait_. This trait will be named `SpecTrait$sp$C1$C2$C3`, where `$sp$` is a fixed specialization marker and `Cn` is an encoding of the fully qualified name of `SpecType(Tn)`. This means that the name of the specialized interface trait reflects in its name all specializing supertypes of the type arguments passed to specialized type parameters in sequence. Arguments passed to non-specialized type parameters are ignored.

The presence of an anonymous class instance creation like `new Vec[T](elems) {}` in the program implies the creation of both a specialized interface trait and a _specialized instance class_, named `Vec$impl$TN`. The class name derives from the trait name by replacing `$sp$` with `$impl$`. 

Should the `SpecType` of all of the concrete type arguments to a specialization be a top class or `Nothing`, a specialized interface trait is not produced. However, should such a specialization require an instance class, an instance class will be created and will be named `SpecTrait$impl`. This is advantageous as it means that anonymous class instances extending specialized traits are _always_ replaced by an instance class.

Replacement of specializations with their corresponding specialized interface traits and anonymous specialized instances with their corresponding specialized instance classes happens via erasure. The _erasure_ of `SpecTrait[T1, T2, T3, ...]` where `SpecType(Tn) = Cn` is:

 - If, for all n, `Cn` is one of the top classes `Any`/`AnyRef`/`AnyVal`/`Object`, or `Cn` is `Nothing`, the usual erased trait `Vec`.
 - If for at least one n, `Cn` is some other class, the corresponding specialized interface trait `SpecTrait$sp$C1$C2$C3`.

We also erase `new SpecTrait[T1, T2, T3, ....]` to:
 - `SpecTrait$impl()` if all type arguments are top classes or nothing.
 - `SpecTrait$impl$C1$C2$C3()` otherwise.

The specialized interface traits and implementation classes are created on demand the first time the corresponding specialization or anonymous class is mentioned in the program.

The idea of this transformation is that we obey the invariant `T <:< Foo[Int] <=> T <:< Foo$sp$Int`.

### Specialized interface traits definition

A specialized interface trait takes the form:

```scala
inline trait Vec[T: Specialized] extends Seq[T]
inline trait Vec$sp$Int extends Vec[Int], Seq$sp$Int
```

In general a specialized interface trait that specializes an inline trait `A[T]` with type argument `S`:
 - drops all `Specialized` trait parameters of `A`
 - adds `A[S]` as first parent trait
 - _also_ adds all parents of `A` *in their specialized forms*,
 - contains declarations from the body of `A` specialized to the type(s) in question (via inline trait inlining)
 - is also an `inline trait`. This is for consistency; see [1].
 - does not take value parameters (including evidence parameters)

We also allow 2 variants of "partial specialization":
```scala
inline trait Vec[T: Specialized, S] // Specialized traits defined with some non-specialized parameters
val x = new Vec[Int, String]() {}

inline trait Foo[T: Specialized, S: Specialized] 
inline trait Bar[T: Specialized] extends Foo[T, Int] // Specialized to a specialized type variable.
class Baz extends Bar[Boolean]
```
In the first of these cases, the generated specialized interface traits will get type parameters:
```scala
inline trait Vec$sp$scala$Int[S]() extends Vec[Int, S]
val x = new Vec$impl$scala$Int()
```

In the other case, specialization waits until all the types are known, except for `Bar` which gets an additional `Foo$sp$Any$Int` parent. <!-- I think the Foo$sp$Any$Int is not strictly necessary because it shouldn't be possible to generate an interface with that type, so maybe we can get rid of it. It may be the case that we can change the condition for specialization from "at least one type is known" (material specialization) to "all types are known" (complete specialization). -->
```scala
inline trait Bar[T: Specialized] extends Foo[T, Int], Foo$sp$Any$Int
inline trait Foo$sp$Any$Int extends Foo[Any, Int]
inline trait Foo$sp$Boolean$Int extends Foo[Boolean, Int]
inline trait Bar$sp$Boolean extends Bar[String], Foo$sp$Boolean$Int
class Baz extends Foo, Bar, Foo$sp$Boolean$Int, Bar$sp$Boolean
```

### Specialized instance class traits definition

A specialized instance class for an inline trait `A` at specialized argument `S`
 - repeats the value parameters of inline trait `A`
 - extends `A[S]` with these parameters
 - extends the corresponding `$sp$` trait

For example, here is the specialized instance class for `Vec` at `Int`:

```scala
class Vec$impl$Int(elems: Array[T]) extends Vec$sp$Int, Vec[Int](elems)
```
The bodies of the interface traits and instance classes are completed by inlining from the inline traits that they extend.

## Relationship to other language features 

### Variance and Specialized Traits
Specialized traits may define variance parameters e.g.:
```scala
inline trait MyFunction1[-T1: Specialized, +R: Specialized]
```
However, variance works in Scala because type parameters are erased, and so we can freely cast e.g. List[Lion] to List[Animal] at runtime. Because specialized traits have a special erasure (and necessarily, because this is how we get the specialization), a couple of variance patterns that are possible with standard traits are not possible with specialized traits. This concerns the top classes and `Nothing`.

In the contravariance case:
```scala
inline trait RecyclingBin[-T: Specialized]:
    def recycle(x: T) = println(s"Recycling ${x}")

def recycleAnInteger(rbin: RecyclingBin[Int]) = 
    rbin.recycle(100)

recycleAnInteger(new RecyclingBin[Any]() {})    // RecyclingBin[Any] can be interpreted as RecyclingBin[Int] due to contravariance
recycleAnInteger(new RecyclingBin[AnyVal]() {}) // RecyclingBin[AnyVal] can be interpreted as RecyclingBin[Int] due to contravariance

// Yet, this erases to:
def recycleAnInteger(rbin: RecyclingBin$sp$Int) = 
    rbin.recycle(100)

recycleAnInteger(RecyclingBin$impl().asInstanceOf[RecyclingBin$sp$Int]) // RecyclingBin$impl cannot be cast to RecyclingBin$sp$Int; Any and Int live in different erasure "buckets" so their erased types are unrelated
recycleAnInteger(RecyclingBin$impl{}.asInstanceOf[RecyclingBin$sp$Int]) // RecyclingBin$impl cannot be cast to RecyclingBin$sp$Int
```
Therefore we have the following issues:
- `RecyclingBin[Any]`, `RecyclingBin[AnyVal]` may not be passed to `RecyclingBin[Int]` whereas normally they would be able to be passed
- `RecyclingBin[Any]`, `RecyclingBin[Object / AnyRef]` may not be passed to `RecyclingBin[Paper]` whereas normally they would.

These uses of contravariance can never succeed. Rather than allowing them to always fail at runtime, we reject them at compilation time. We impose an additional restriction on contravariance with specialized parameters:
- If (when `F1 >:> F2`) `A[F1]` is to be interpreted as `A[F2]` under `A[-T: Specialized]`, we require that `SpecType(F1) = SpecType(F2)`. Given `F1 >:> F2` and looking at the definition of `SpecType` this means concretely:
  - `RecyclingBin[Any]`, `RecyclingBin[AnyVal]` may not be passed to `RecyclingBin[Int]` whereas normally they would
  - `RecyclingBin[Any]`, `RecyclingBin[Object / AnyRef]` may not be passed to `RecyclingBin[Paper]` whereas normally they would.
  - `RecyclingBin[Any]` may be passed to `RecyclingBin[Object]`, `RecyclingBin[AnyRef]`, `RecyclingBin[AnyVal]` as these all erase to `RecyclingBin`.

Covariance has a similar problem:
 - In general covariance works fine because it corresponds to interpreting `A[F1]` as `A[F2]` where `F1 <:< F2`. Either`A[F1]` and `A[F2]` both erase to the same type (`A$sp$SpecType(F2)`, or `A` if `F1` and `F2` are both top classes), or `A[F1]` erases to `A$sp$F1` and `A[F2]` erases to `A` (`F2` is a top class while `F1` is not). But `A$sp$F1` is a subtype of `A` by definition so the upcast will succeed (and upcasts are generally cheap compared to downcasts on the JVM so this is acceptable from a performance perspective).  
 - The only exception is with `Nothing`, because we want to interpret `A[Nothing]` as e.g. `A[Int]`, but we erase `A[Nothing]` to `A`. `A >:> A$sp$Int` so this doesn't work and so we also have to reject covariance using `Nothing`. To implement this correctly would require a specialization `A$sp$Nothing` which extends every single specialization in the program, and this was deemed infeasible. The lack of covariance with `Nothing` makes the code less ergonomic in some cases. For example `case object Nil extends List[Nothing]` has to become `inline trait Nil[T: Specialized] extends List[T]` and `inline def apply[T: Specialized] = new Nil[T] () {}`, but we don't lose too much expressivity.


### `sealed` Specialized traits
Like other traits, specialized traits may be `sealed`, but this requires some thought. In particular, the generated `$sp$` traits and `$impl$` classes extend the `sealed` specialized trait potentially from another file without the user being aware that this is happening behind the scenes. We want to allow for example:

```scala
// (1)
// A.scala
sealed inline trait Foo[T: Specialized]

// B.scala
def foo(x: Foo[String])
```
because this would be allowed with ordinary traits, but the desugaring creates `Foo$sp$String` which is arguably an illegal child of the sealed `Foo[String]` as it's produced only when compiling B.scala.

We are also faced with the question of whether we should allow the following:
```scala
// (2)
// A.scala
sealed inline trait Foo[T: Specialized]
val x = new Foo[Int]() {} // Forces creation of Foo$sp$Int and Foo$impl$Int

// B.scala
val y = new Foo[Int]() {}
```
In the bytecode `x` and `y` may or may not point to the same `Foo$impl$Int` depending on if we share the generated specialized classes, but in 
the source code `y` extends `Foo[Int]` illegally.

In both cases we essentially opt for the "source code" interpretation as this seems clearest for users. In particular we allow example (1) but not example (2). This happens automatically because `sealed` trait inheritance checking is done before specialized trait desugaring / erasure, so it is unaware of the `$sp$` traits and `$impl$` classes. 

It is tempting to think that since the `$impl$` classes do not exist in source we may be able to allow the second example, but this is dangerous for exhaustivity checking:

```scala
// A.scala
sealed inline trait Foo[T: Specialized]
inline trait Bar[T: Specialized] extends Foo[T]

def foo(x: Foo[Int]) = x match {
    case y: Bar[Int] => println("All good!")
}

// B.scala
val y = new Foo[Int]() {} // Bad; A.scala compiled with no warnings as exhaustivity checker assumed this was impossible
foo(y)
val z = new Bar[Int]() {} // Ok: We allow this because Bar is not sealed so the exhaustivity checking in A.scala was correct. 
foo(z)

```

In terms of exhaustivity checking, we also want this to work within a single file. Consider:
```scala
sealed inline trait List[+T: Specialized]
sealed inline trait Nil[T: Specialized] extends List[T]
sealed inline trait :+:[T: Specialized](h: T, t: List[T]) extends List[T]

val xs: List[Double] = new Nil[Double]() {}

def foo(x: List[Double]): Unit = x match {
  case xs: :+:[_] => println("Cons case")
  case _: Nil[_] =>  println("Nil case")
  // warning: non-exhaustive pattern match, missing case _: List[Double]
}
```

Without any changes, the anonymous class `Nil[Double]` also extends the `List[Double]` interface (because anonymous class instances mixin all ancestor traits if there are parameters to pass), so pattern match exhaustivity checking on `List[Double]` requires `_: List[Double]` because the List trait has anonymous class children and so we are unable to decompose `_: List[Double]` to children in space checking. This occurs because we don't do the `$impl$` class replacements until erasure so the anonymous class is still around. For that reason we exempt anonymous classes extending specialized traits from being treated as children for pattern match exhaustivity checking, but we do treat the `$impl$` classes as children (the fact that they are defined even if unused suffices for them to be taken into account). This is correct because the anonymous classes will not exist at runtime when the pattern matches run, rather having been replaced by the `$impl$` classes.

Furthermore, the generated `List$sp$Double` trait also interferes. After we fix this issue:

```scala
def foo(x: List[Double]): Unit = x match {
  case xs: :+:[_] => println("Cons case")
  case _: Nil[_] =>  println("Nil case")
  // warning: non-exhaustive pattern match, missing case _: List[Double] & List$sp$Double
}
```
There are two potential solutions to this:
- make `$sp$` traits inherit `sealed` if the original specialized trait is `sealed`, but this may make future sharing of specializations challenging if we want to extend the specialized traits from another file.
- don't register the `$sp$` traits as children of the original specialized trait for exhaustivity checking. This is safe because these traits are synthetic and we have the invariant that `T <:< Foo[Int] <=> T <:< Foo$sp$Int`, so users cannot match on `Foo$sp$Int` or `Foo[Int] minus Foo$sp$Int` (the latter being empty)

We opt for the latter.

### Other implementation restrictions on specialized traits
These could be lifted with additional work.

| Behaviour                | Limitation | Chance of fixing / limited by |
|--------------------------|-----------------------------------------------|---|
| Use of `?` bounds | May not be used for Specialized parameters; however may be used for non-Specialized parameters in specialized traits. | It was thought that this would be challenging because it would require bridge methods that "despecialize" to ensure that `A$sp$Int` implements `A`'s interface. In the end these bridge methods have been implements, so it should be very possible to lift this restriction as future work.|
| Defining specialized traits inside traits/classes/objects | May define specialized traits inside `object`s. May not define them inside `class`es or `trait`s. | Limited by the way we flatten the owners of generated `$impl$` classes and `$sp$` traits. We really want to build the `Foo$impl$` class directly next to `Foo`. For a single CU this would be possible if we walked the entire tree and found where these belong; for multiple CUs it is more complicated as the tree may not exist in the current CU. The case of path dependent specialized traits was deemed niche enough to not be high priority, and for multiple CUs seems very tricky. |

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

val x = new Seq[Int](Array(1, 2, 3)) {}
```

This generates the following interface traits (after inlining, conversion to pure interfaces and erasure):

```scala
inline trait Iterator$sp$Int extends Iterator:
  def hasNext: Boolean
  def next(): Int

inline trait ArrayIterator$sp$Int extends ArrayIterator, Iterator$sp$Int:
  def hasNext(): Boolean
  def next(): Int

inline trait Iterable$sp$Int extends Iterable:
  def iterator: Iterator$sp$Int
  def forall(f: Int => Unit): Unit

inline trait Seq$sp$Int extends Seq, Iterable$sp$Int:
  def forall(f: Function1): Unit
  def length(): Int
  def apply(i: Int): Int
  def iterator(): Iterator$sp$Int
```
Note that these traits repeat the parent types of their corresponding inline traits (but with specialization added). For instance, `ArrayIterator$sp$Int` extends the specialized version of its parent `Iterator$sp$Int`, so the specialized trait may be used in contexts expecting:

- The specialized trait `ArrayIterator$sp$Int` itself (i.e. `ArrayIterator[Int]` in source code)
- A generic `ArrayIterator` (i.e. `ArrayIterator[?]` in source code)
- Specialized traits higher in the specialized hierarchy for example `Iterator$sp$Int`.

The specialized implementation classes for `ArrayIterator` and `Seq` are as follows (since `new Seq[Int] {}` is found in the program. This triggers creation of `Seq$sp$Int` which triggers the need for `ArrayIterator$sp$Int`):

```scala
class ArrayIterator$impl$Int(elems: Int[]) extends ArrayIterator$sp$Int, ArrayIterator(elems):
  private var current = 0
  override def hasNext: Boolean =
    current < elems.length
  override def next(): Int =
    try elems(current) finally current += 1

class Seq$impl$Int(elems: Int[]) extends Seq$sp$Int, Seq(elems):
  override def iterator: Iterator$sp$Int = new ArrayIterator$impl$Int(elems)
  override def forall(f: Int => Unit): Unit =
    val it: Iterator$$sp$Int = this.iterator()
    while it.hasNext do f.apply$mcVI$sp(it.next())
  override def length: Int = elems.length
  override def apply(i: Int): Int = elems(i)
```

Here, `f.apply$mcVI$sp` is the specialized apply method of `Function1` at type `Int => Unit`.
This method is generated by Scala 2's function specialization which is also adopted by Scala 3.

Erasure also adds some bridge methods which are necessary to maintain interface compatibility. For example:
```scala
// in class Seq$impl$Int
override def iterator(): Iterator = this.iterator()
```

This is especially important in the case where the specialized trait appears as a method parameter. For example:
```scala
inline trait Foo[T: Specialized]

inline trait A[T: Specialized]:
  def foo(x: Foo[T]): Int

class B extends A[Int]:
  override def foo(x: Foo[Int]) = 10

// erases to:
inline trait A: 
  def foo(x: Foo): Int
class B extends A A$sp$Int:
  override def foo(x: Foo$sp$Int): Int = 10
  override def foo(x: Foo): Int = this.foo(x.asInstanceOf[Foo$sp$Int]) // Bridge method maintains interface compatibility with A
```
This cast is fine because of the invariant that  `T <:< Foo[Int] <=> T <:< Foo$sp$Int`, since the method was typechecked under `Foo[Int]`.

## Transportation of Specialized through generic code
The `Specialized` Type Class is erased at runtime. Instances of `Specialized[T]` are created automatically for types that do not contain type variables.

It may surprise you to note that the following is valid. The `Numeric` constraint on `T` is only checked when
a concrete type is provided for `S` (and by extension `T`) when instantiating `T2`. 
```scala
inline trait T1[T: Numeric]
inline trait T2[S] extends T1[S]
```
In constrast, we do not allow this type of behaviour for `Specialized`. This is largely to avoid confusion. In particular:
```scala
inline trait T1[T: Specialized]
inline trait T2[S] extends T1[S]
val x = new T2[Int]() {}
```

Should `x` be a Specialized `$impl$` instance or a normal anonymous class? If we naively look at just `S`'s definition we would say an anonymous class,
but this is complicated by the fact that the generated anonymous class will also mixin `T1` directly as well as `T2`. Could we specialize
just for `T1`? This is hard to imagine. Furthermore if we don't specialize, this is also counter intuitive because T1 is then never specialized
to Int, because we have no specialized instance of trait `T2` to inline into.

Therefore we require users to explicitly transport `Specialized` through their code. If a type is used in the right hand side of an `extends` clause then it must also be specialized at its definition. This also applies to type variables appearing deeper inside types. 

Furthermore, a specialized method taking a type parameter `T` and invoking specialized traits or calling other specialized methods using `T` must declare this `T` as specialized. This ensures that a specialized 'slot' can only be filled by a concrete type or a type defined in terms of specialized type parameters.

```scala
inline trait T1[T: Specialized]

inline trait T2[S] extends T1[S]                                // error: S should be specialized
inline trait T3 extends T1[List[Int]]                           // ok
inline trait T4[S] extends T1[List[S]]                          // error: S should be specialized
inline trait T5[S: Specialized] extends T1[List[S]]             // ok
inline trait T6[T[_], S] extends T1[T[S]]                       // error: T should be specialized // error: S should be specialized

inline def foo1[S](x: T1[S]): Int = 10                          // error: S should be specialized
inline def foo2[S](x: T1[List[S]]): Int = 10                    // error: S should be specialized
inline def foo3[S: Specialized](x: T1[List[S]]): Int = 10       // ok

inline def bar1[S] = new T1[S]() {}                             // error: S should be specialized
inline def bar2[S] = new T1[List[S]]() {}                       // error: S should be specialized
inline def bar3[S: Specialized] = new T1[List[List[S]]]() {}    // ok
```

Note that in the cases with `List` this restriction is imposed not because of the definition of `List`, but rather simply because we have a 
type variable `S` which is not marked as Specialized which appears 'somewhere inside' a type in a Specialized position. Again this is done to
avoid confusion. A user extending `T1[List[S]]` would likely expect some degree of specialization to given the definition of `T1`, but this 
is not possible if `S` is not marked as `Specialized`. In practice `specType(List[_])` is `Any` however.

## Specialized Traits in the Compiler
We introduce a new phase `desugarSpecializedTraits` responsible for detecting specializations, generating the necessary `$sp$` and `$impl$` 
classes for these specializations, and inlining into them.

The replacement of references to e.g. `Vec[Int]` with `Vec$sp$Int` and `new Vec[Int] {}` with `Vec$impl$Int()` is done at erasure, because we cannot change signatures before then, and doing it afterwards would not prevent the boxing that we seek to avoid. 

Specialized traits rely on the semantics and implementation of inline traits, so it may seem logical that `desugarSpecializedTraits` would merely generate
the prototypes for the classes, and allow `specializeInlineTraits` to inline the bodies. We *do not* do this. Rather, the phase `desugarSpecializedTraits` directly performs inlining of the parent traits (`Vec[Int]` in the above example) into the
generated `$sp$` traits and `$impl$` classes, sharing the relevant code with the `specializeInlineTraits` phase through
the `Inlines.scala` file. 

This is done because while we want to keep the two phases separate and avoid coupling where possible (thus allowing e.g. specialized traits to be disabled 
while maintaining inline traits), implementing specialized traits requires being able to alternate
 between the inlining and specializing steps in some cases. Consider the following
example:

```scala
inline trait D[R: Specialized]

inline trait C[S: Specialized]:
   def w(y: D[S]): Unit = println("w")

inline trait A[T: Specialized]:
   def x(y: C[T]): Unit = println("x")

class B extends A[Char]
```
If we run just the specialization part of the specialized trait processing (without the inlining yet), we get:

```scala
inline trait A$sp$Char extends A[Char]
```

Inlining the body of `A` results in:
```scala
inline trait A$sp$Char extends A[Char]:
  def x(y: C[Char]): Unit = println("x")
```
Notice that this generates a reference to `C[Char]` which ought to be specialized, given that `S` is marked as `Specialized` in 
the definition of `C`. Therefore we need to run the specialization process again. This time we will generate:
```scala
inline trait C$sp$Char extends C[Char]
```
Inlining the body of `C` into this trait will create a reference to `D[Char]`, which also ought to be specialized. Thus it is clear that
it is possible to create arbitrarily long chains requiring alternating between specialized trait generation and inline trait inlining. We note that 
this problem arises not only with the `$sp$` traits, but also the `$impl$` classes (see `specialized-trait-inlining-causes-implementation-required.scala`).

To resolve this problem without alternating between and looping the `specializeInlineTraits` and `desugarSpecializedTraits` phases in an inconvenient way, we opt to make:
- `specializeInlineTraits` responsible for inlining inline traits written directly in user code:
   - If a specialized trait creates an inline trait inlining opportunity which is not `specialized`, this is dealt with by `specializeInlineTraits`. 
   - Further if a user writes `class Bar extends Foo[Int]` where Foo is declared Specialized, `specializeInlineTraits` will do the inlining. 
- `desugarSpecializedTraits` responsible for finding specializations and generating the required `$sp$` traits and `$impl$` classes, inlining the parent specialized traits into these traits/classes, and repeating until no more inlining can be performed and no more `$sp$` traits and `$impl$` classes are needed.

This decouples the responsibilities of the two phases, but given that `desugarSpecializedTraits` could produce a reference that requires inline trait inlining
in the child class (e.g. `tests/run/specialized-trait-requires-inline-trait-inlining.scala`) we need to run `desugarSpecializedTraits` first.

We also need to introduce a miniphase `pruneSpecializedMethods` which removes specialized methods. These methods are inline so they would be removed later anyway, but we need to remove them before `pruneInlineTraits` to avoid potentially creating dangling references.

## [1] Why are the generated traits inline?
Consider the following:
```scala
inline trait A[T]:
	def foo = "Hello World"
inline trait B extends A[Int]
class C extends B
// inlines to:
class C extends B:
	def foo = "Hello World"

// vs...

inline trait A[T: Specialized]:
	def foo = "Hello World"
inline trait B extends A[Int]
class C extends B
// would expand to:
trait A$sp$Int:
	def foo = "Hello World"
inline trait B extends A$sp$Int
class C extends B
```
We consider the fact that the location of the inlined `foo` method changes with only a simple
addition of `Specialized` to be inconsistent / confusing. Furthermore it would violate the rule
that ordinary traits may not extend inline traits, and causes problems with partial specialization:
```scala
// (1)
inline trait A[T: Specialized, D: Specialized]:
  def foo: T
  def bar: D
inline trait B[S: Specialized] extends A[S, Int]
trait C extends B[Char]

// would expand to:
trait A$sp$S$Int[S: Specialized] extends A[S, Int]: // (Ignoring the fact that Specialized may not be used on ordinary traits).
  def foo: S
  def bar: Int
inline trait B[W: Specialized] extends A$sp$S$Int[W]
trait C extends B[Char]
```
The definitions are stuck in `A$sp$S$Int$` because it is not inline. This means we can never usefully specialize on `W` even though it is declared `Specialized`. 

Because we make the generated traits inline, we modify the behaviour of inline traits relative to the original semantics from Timothée's thesis, such that inline traits extended by other inline traits are still inlined (instead of inlining only at the first ordinary class extending the family of inline traits). This is necessary so that `A$sp$S$Int` can be made inline and still contain the specialized declarations which we need when we use it as an interface. The original argument for only inlining at the bottom of the hierarchy was to reduce code generation, and that this was sufficient when we only have inline traits, however the additional code generation is only linear in the number of traits in the sequence (*and limited to the interfaces since the implementations are pruned*) as we do not inline multiple copies. We consider this acceptable in order to implement specialized.

## Future Work

### Caching of Specialized Traits and Classes

To avoid redundant repeated code generation of the same traits and classes, specialized instance traits and classes could be cached. 
The compiler would put their tasty and classfile artifacts in a special directory on the class path. Each artifact would contain in an annotation a hash of the  contents of the trait from which the instance was derived. Before creating a new specialized instance, the compiler would consult this directory to see whether an instance with the given name exists and whether its hash matches. In that case, the artifacts can be re-used.

### Improve Existing Class Hierarchies

We have shown that we can formulate an alternative version of a collection-like class hierarchy that is fully specialized. But can we retro-fit this idea even to existing collections? The direct approach would
clearly not work since an existing collection like `Vector[T]` can be created from anywhere whereas a specialized collection can be created only in a monomorphic context where we know the type instance of `T`. So specialized
collections come with a tax in expressiveness which pays for their superior performance.

But it turns out we can gain a lot of flexibility with three additional tweaks to the language and compiler.

#### 1. Adapt Overloading to Specialization

More flexibility could be gained if we allowed method overloading between specialized inline methods and normal methods with matching type signatures. For instance, the `Vector` implementation above seriously restricts `map` by requiring that its `B` type parameter is also `Specialized`. Thus `map` cannot be used to map a specialized collection to another collection if the result element type is not statically known. But we could alleviate the problem by allowing a second, overloaded `map` operation like this:
```scala
  def map[B](f: A => B): collection.immutable.Vector[B] =
    new collection.immutable.Vector[B](elems.map(f))
```
The second implementation of `map` will return an unspecialized vector if
the new element type is not statically known. If overloads like this were allowed, they could be resolved by picking the specialized inline version if
a `Specialized` instance can be synthesized for the actual type argument, and picking the unspecialized version otherwise.

We can do even better if we allow some additions of the existing collections. In that case, we can add definitions like the inline `map` above to the original collections.
That means, whenever we have a collection `xs` with a type such as `Vector[A]` and a function `f` with a statically known result type `B`, then `xs.map(f)` returns a specialized collection. So we can get specialized collections out of normal collections as long as the element type of the created collection is statically known.

This can be generalized. In particular, all `apply` methods of `Vector` should be split into methods taking specialized types and unrestricted methods. For instance:
```scala
object Vector:
  def apply[T](xs: T*): Vector[T] = ...
  inline def apply[T: Specialized](xs: T*): faster.Vector[T] = ...
```
The same holds for all collection methods such as `map` that return a new collection of a different element type.

#### 2. Automate the Boilerplate with `specializedBy`

The described scheme would entail some amount of code duplication. We could automate this with a new annotation that is put on a class and states that the class has a specialized variant. Example:
```scala
@specializedBy[faster.Vector] class Vector[+T] ...
```
If a class carries such an annotation the specialized inline functions described above could be added automatically.

#### 3. Optimize Use Sites by Path Splitting

One remaining problem is that specialization is a compile-time operation. Without putting in additional work, we cannot immediately exploit the situation where a runtime type is a specialized collection but the static type is unspecialized. For instance, consider this use of `Vector`:

```scala
def sumElems(xs: Vector[Int]): Int =
  var i = 0
  var sum = 0
  while i < xs.length do
    sum += xs(i)
    i += 1
  sum
```
Here, the problem is that, even though we know that `xs` is a `Vector` of `Int`, we cannot deduce that has been specialized to a `faster.Vector[Int]`. Therefore, `xs(i)` goes through the `apply` method of `Vector`. If the runtime class of `Vector` is indeed specialized this would box the `Int` element to `Object` in a bridge method and unbox it again to `Int` at the call site. This could lose a lot of performance, unless the JVM manages to optimize the box/unbox pair away (so far, experience shows that the JVM is not very good at this). The performance could be even worse than working with an unspecialized `Vector` where elements are held in boxed form so they don't have to be boxed each time they are accessed.

Of course, we can narrow the type of `sumElems` to
```scala
def sumElems(xs: faster.Vector[Int]): Int
```
but that would make it less generally usable. Another alternative is to optimize `sumElems` by path splitting. We could detect at runtime whether
`xs` is a `faster.Vector` and optimize the code if it is. For instance, like this:
```scala
def sumElems(xs: Vector[Int]): Int =
  val faster: faster.Vector[Int] | Null = xs match
    case xs: faster.Vector[_] => xs
    case _ => null
  var i = 0
  var sum = 0
  while i < xs.length do
    sum += (if faster != null then faster(i) else xs(i))
    i += 1
  sum
```
That would avoid the boxing at the cost of a type test in the computation of `faster` and a null test in the call of `apply`. The type test would be amortized over possibly many calls in the loop. We could do even better by generating a bit more code, splitting the whole loop:
```scala
def sumElems(xs: Vector[Int]): Int =
  val faster: faster.Vector[Int] | Null = xs match
    case xs: faster.Vector[_] => xs
    case _ => null
  var i = 0
  var sum = 0
  if faster != null then
    while i < xs.length do
      sum += faster(i)
      i += 1
  else
    while i < xs.length do
      sum += xs(i)
      i += 1
  sum
```
The example has shown that one can write code over possibly specialized collections that is both general and highly performant. But it does require a lot of hand-written boiler-plate.

The boilerplate could be generated automatically by an optimization phase in the compiler. Essentially, when compiling methods that take parameters whose type is a class annotated with `specializedBy`, we can do the path splitting automatically in an optimization step. The optimization would first analyze the body of the method to decide which path splitting strategy to apply.

We believe the three tweaks we have outlined could overcome most of the performance penalties imposed by existing unspecialized class hierarchies like collections, making their performance comparable to languages that use global monomorphization.

### Specializing Tuples

The same optimizations can also avoid boxing for tuple elements, and with it extractor-based pattern matching. Scala 3 does not currently specialize tuples at all. Scala 2 specializes pairs but not tuples of higher arity. But it uses a scheme quite different from the one proposed here.

Scala 2 pre-generates pair classes for all combinations of primitive types and Object. Each pair class inherits or implements access methods for all primitive types and Object. This allows to
arrange it so that access always goes through a specialized method that does not involve boxing. No path splitting is needed to achieve that. On the other hand, the exponentially growing amount of code that needs to be generated restricts the scheme to pairs only. Also, specialization is not done for reference types, access to fields of (say) `String` type still need a cast from `Object` to `String`.

We could adopt the Scala 2 specialization scheme for pairs. This is not hard, since no new classes need to be generated, we simply re-use the Scala 2 classes. Then the new specialization scheme would apply to tuples of higher arities. Or we forego Scala 2 specialization altogether and specialize all tuples with the new scheme.

The situation with functions is a bit different. Here, Scala 2 specializes functions with up to two parameters, and Scala 3 re-uses these specializations.
Going beyond that requires some adaptations since functions are not implemented as classes but as lambdas that are directly supported by the JVM. So Scala 3 specialization would have to be extended to the definition of these lambdas.

### Hand-written Specializations

Additional improvements could be gained if we allowed the programmer to pick their own implementations for specialized class instances. For example,
we could have a
```scala
inline trait HashMap[K: Specialized, +V: Specialized] ...
```
and an optimized sub-trait
```scala
inline trait IntHashMap[+V: Specialized] extends HashMap[Int, V] ...
```
The implementation in `IntHashMap` could exploit that fact that the key type `K` is known to be `Int` to pick a more performant algorithm, for instance.

It would be great if we could use `IntHashMap` each time a specialized HashMap such as `HashMap$sp$Int$String` is referred to or created. In other words, `IntHashMap` should act as a drop-in replacement for `HashMap$sp$Int$String` that is selected automatically. A detailed proposal for this is left for future work.
