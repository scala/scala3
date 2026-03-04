---
layout: doc-page
title: "Capture Checking of Classes"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/classes.html
---

## Introduction

The principles for capture checking closures also apply to classes. For instance, consider:
```scala sc:nocompile
class Logger(using fs: FileSystem):
  def log(s: String): Unit = ... summon[FileSystem] ...

def test(xfs: FileSystem): Logger^{xfs} =
  Logger(xfs)
```
Here, class `Logger` retains the capability `fs` as a (private) field. Hence, the result
of `test` is of type `Logger^{xfs}`

Sometimes, a tracked capability is meant to be used only in the constructor of a class, but
is not intended to be retained as a field. This fact can be communicated to the capture
checker by declaring the parameter as `@constructorOnly`. Example:
```scala sc:nocompile
import annotation.constructorOnly

class NullLogger(using @constructorOnly fs: FileSystem):
  ...
def test2(using fs: FileSystem): NullLogger = NullLogger() // OK
```

The captured references of a class include _local capabilities_ and _argument capabilities_. Local capabilities are capabilities defined outside the class and referenced from its body. Argument capabilities are passed as parameters to the primary constructor of the class. Local capabilities are inherited:
the local capabilities of a superclass are also local capabilities of its subclasses. Example:

```scala sc:nocompile
class Cap extends caps.SharedCapability

def test(a: Cap, b: Cap, c: Cap) =
  class Super(y: Cap):
    def f = a
  class Sub(x: Cap) extends Super(x)
    def g = b
  Sub(c)
```
Here class `Super` has local capability `a`, which gets inherited by class
`Sub` and is combined with `Sub`'s own local capability `b`. Class `Sub` also has an argument capability corresponding to its parameter `x`. This capability gets instantiated to `c` in the final constructor call `Sub(c)`. Hence,
the capture set of that call is `{a, b, c}`.

## Capture Set of This

The capture set of the type of `this` of a class is inferred by the capture checker, unless the type is explicitly declared with a self-type annotation like this one:
```scala sc:nocompile
class C:
  self: D^{a, b} => ...
```
The inference observes the following constraints:

 - The type of `this` of a class `C` includes all captured references of `C`.
 - The type of `this` of a class `C` is a subtype of the type of `this`
   of each parent class of `C`.
 - The type of `this` must observe all constraints where `this` is used.

For instance, in
```scala sc:nocompile
class Cap extends caps.SharedCapability
def test(c: Cap) =
  class A:
    val x: A = this
    def f = println(c)  // error
```
we know that the type of `this` must be pure, since `this` is the right hand side of a `val` with type `A`. However, in the last line we find that the capture set of the class, and with it the capture set of `this`, would include `c`. This leads to a contradiction, and hence to a checking error:
```
   |    def f = println(c)  // error
   |                    ^
   |       reference (c : Cap) is not included in the allowed capture set {}
   |       of the enclosing class A
```

### Traits and Open Classes

The self-type inference behaves differently depending on whether all subclasses of a class are known. For a regular (non-open, non-abstract) class, all subclasses are known at compile time,¹ so the capture checker can precisely infer the self-type. However, for traits, abstract classes, and [`open`](../../other-new-features/open-classes.md) classes, arbitrary subclasses may exist, so the capture checker conservatively assumes that `this` may capture arbitrary capabilities
(i.e., it infers the universal capture set `any`).

¹We ignore here the possibility that non-open classes have subclasses in other compilation units (e.g. for testing) and assume that these subclasses do not change the inferred self type.

For example (assuming all definitions are in the same file):
```scala sc:nocompile
class A:
  def fn: A = this   // ok

trait B:
  def fn: B = this   // error
  def fn2: B^ = this // ok

abstract class C:
  def fn: C = this   // error
  def fn2: C^ = this // ok

sealed abstract class D:
  def fn: D = this   // ok

object D0 extends D

open class E:
  def fn: E = this   // error
  def fn2: E^ = this // ok
```

### Inheritance

The capture set of `this` of a class or trait also serves as an upper bound of the possible capture
sets of extending classes
```scala sc:nocompile
abstract class Root:
  this: Root^ => // the default, can capture anything

abstract class Sub extends Root:
  this: Sub^{a, b} => // ok, refinement {a, b} <: {any}

class SubGood extends Sub:
  val fld: AnyRef^{a} = a // ok, {a} included in {a, b}

class SubBad extends Sub:
  val fld: IO^{io} = io // error, {io} not included in the this capture set {a, b}

class SubBad2 extends Sub:
  this: SubBad2^{io} => // error, self type SubBad2^{e} does not conform to Sub^{c, d}
```

Generally, the further up a class hierarchy we go, the more permissive/impure the `this` capture set
of a class will be (and the more restrictive/pure it will be if we traverse the hierarchy downwards).
For example, Scala 3's top reference type `AnyRef`/`Object` conceptually has the universal
capability
```scala sc:nocompile
class AnyRef:
  this: AnyRef^ =>
  // ...
```
Similarly, pure `Iterator`s are subtypes of impure ones.

## Capture Tunneling

Consider the following simple definition of a `Pair` class:
```scala sc:nocompile
class Pair[+A, +B](x: A, y: B):
  def fst: A = x
  def snd: B = y
```
What happens if `Pair` is instantiated like this (assuming `ct` and `fs` are two capabilities in scope)?
```scala sc:nocompile
def x: Int ->{ct} String
def y: Logger^{fs}
def p = Pair(x, y)
```
The last line will be typed as follows:
```scala sc:nocompile
def p: Pair[Int ->{ct} String, Logger^{fs}] = Pair(x, y)
```
This might seem surprising. The `Pair(x, y)` value does capture capabilities `ct` and `fs`. Why don't they show up in its type at the outside?

The answer is capture tunneling. Once a type variable is instantiated to a capturing type, the
capture is not propagated beyond this point. On the other hand, if the type variable is instantiated
again on access, the capture information "pops out" again. For instance, even though `p` is technically pure because its capture set is empty, writing `p.fst` would record a reference to the captured capability `ct`. So if this access was put in a closure, the capability would again form part of the outer capture set. E.g.
```scala sc:nocompile
() => p.fst : () -> Int ->{ct} String
```
In other words, references to capabilities "tunnel through" in generic instantiations from creation to access; they do not affect the capture set of the enclosing generic data constructor applications.
This principle plays an important part in making capture checking concise and practical.

## Captures of New

Consider the following class, assuming we have capabilities `io`, `async`, and `out` in our environment:
```scala sc:nocompile
class C(x: () => Unit):
  val f: File^{io} = File()
  def g() =
    out.println("one")
    f.write("two")
    x()
```
What is the capture set of a class creation expression `C(y)`, assuming `y` has type `() ->{async} Unit`? This capture set computed from _local_ and _external_ elements. The local elements are:

 - all capabilities passed in parameters, in this case `y` if it is a value, or its underlying capability `async` otherwise, and
 - all capabilities in the types of class fields, in this case `io`, which comes from the type of `f`.

The external elements are all capabilities from outside the class that are referenced
by a method of the class. In this case the external elements are `out` and `io`. `out` is accessed directly from method `g`. `io` is accessed indirectly through the
field `f`.

The external elements of the capture set of a class can be declared explicitly with a `uses` clause:

```scala sc:nocompile
class C(x: () => Unit) uses out, io:
  val f: File^{io} = File()
  def g() =
    out.println("one")
    f.write("two")
    x()
```

A `uses` clause must be given if a class that is visible in other compilation units captures external capabilities. This is to support separate compilation where we need to know the capture set of `C(...)` without analyzing the internals of `C`.

### Captures due to Class Initialization

The previous section described what capabilities get captured by the value of a class instance creation expression. But this is not the only relevant capture set linked with `new`. It's also important to know which capabilities are accessed when a class is initialized.

For example, consider:
```scala sc:nocompile
class D():
  val str: String =
    out.println("str was initialized")
    "abc"
```
Here, the initialization of `D` accesses the capability `out`. Therefore, the function value `() => D()` has type `() ->{out} D`.

The capabilities accessed during initialization of a class can be declared in the class with a `uses_init` clause, like this:

```scala sc:nocompile
class D() uses_init out:
  val str: String =
    out.println("str was initialized")
    "abc"
```
A `uses_init` clause must be given if a class that is visible in other compilation units accesses capabilities during initialization. Like for `uses` clauses, this is to support separate compilation where we need to know the use set of a class instance creation without analyzing the internals of the class.

**Syntax**

`uses` clauses come after `extends` and `derives` clauses. `uses_init` clauses come after `uses` clauses.

```
InheritClauses    ::=  [‘extends’ ConstrApps]
                       [‘derives’ QualId {‘,’ QualId}]
                       [‘uses’ CaptureRef {‘,’ CaptureRef}]
                       [‘uses_init’ CaptureRef {‘,’ CaptureRef}]
```


## A Larger Example

As a larger example, we present an implementation of lazy lists and some use cases. For simplicity,
our lists are lazy only in their tail part. This corresponds to what the Scala-2 type `Stream` did, whereas Scala 3's `LazyList` type computes strictly less since it is also lazy in the first argument.

Here is the base trait `LzyList` for our version of lazy lists:
```scala sc:nocompile
trait LzyList[+A]:
  def isEmpty: Boolean
  def head: A
  def tail: LzyList[A]^{this}
```
Note that `tail` carries a capture annotation. It says that the tail of a lazy list can
potentially capture the same references as the lazy list as a whole.

The empty case of a `LzyList` is written as usual:
```scala sc:nocompile
object LzyNil extends LzyList[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???
```
Here is a formulation of the class for lazy cons nodes:
```scala sc:nocompile
import scala.compiletime.uninitialized

final class LzyCons[+A](hd: A, tl: () => LzyList[A]^) extends LzyList[A]:
  private var forced = false
  private var cache: LzyList[A]^{this} = uninitialized
  private def force =
    if !forced then { cache = tl(); forced = true }
    cache

  def isEmpty = false
  def head = hd
  def tail: LzyList[A]^{this} = force
end LzyCons
```
The `LzyCons` class takes two parameters: A head `hd` and a tail `tl`, which is a function
returning a `LzyList`. Both the function and its result can capture arbitrary capabilities.
The result of applying the function is memoized after the first dereference of `tail` in
the private mutable field `cache`. Note that the typing of the assignment `cache = tl()` relies on the [monotonicity rule](basics.md#monotonicity-rule) for `{this}` capture sets.

Here is an extension method to define an infix cons operator `#:` for lazy lists. It is analogous
to `::` but instead of a strict list it produces a lazy list without evaluating its right operand.
```scala sc:nocompile
extension [A](x: A)
  def #:(xs1: => LzyList[A]^): LzyList[A]^{xs1} =
    LzyCons(x, () => xs1)
```
Note that `#:` takes an impure call-by-name parameter `xs1` as its right argument. The result
of `#:` is a lazy list that captures that argument.

As an example usage of `#:`, here is a method `tabulate` that creates a lazy list
of given length with a generator function `gen`. The generator function is allowed
to have side effects.
```scala sc:nocompile
def tabulate[A](n: Int)(gen: Int => A): LzyList[A]^{gen} =
  def recur(i: Int): LzyList[A]^{gen} =
    if i == n then LzyNil
    else gen(i) #: recur(i + 1)
  recur(0)
```
Here is a use of `tabulate`:
```scala sc:nocompile
class LimitExceeded extends Exception
def squares(n: Int)(using ct: CanThrow[LimitExceeded]) =
  tabulate(10): i =>
    if i > 9 then throw LimitExceeded()
    i * i
```
The inferred result type of `squares` is `LzyList[Int]^{ct}`, i.e it is a lazy list of
`Int`s that can throw the `LimitExceeded` exception when it is elaborated by calling `tail`
one or more times.

Here are some further extension methods for mapping, filtering, and concatenating lazy lists:
```scala sc:nocompile
extension [A](xs: LzyList[A]^)
  def map[B](f: A => B): LzyList[B]^{xs, f} =
    if xs.isEmpty then LzyNil
    else f(xs.head) #: xs.tail.map(f)

  def filter(p: A => Boolean): LzyList[A]^{xs, p} =
    if xs.isEmpty then LzyNil
    else if p(xs.head) then xs.head #: xs.tail.filter(p)
    else xs.tail.filter(p)

  def concat(ys: LzyList[A]^): LzyList[A]^{xs, ys} =
    if xs.isEmpty then ys
    else xs.head #: xs.tail.concat(ys)

  def drop(n: Int): LzyList[A]^{xs} =
    if n == 0 then xs else xs.tail.drop(n - 1)
```
Their capture annotations are all as one would expect:

 - Mapping a lazy list produces a lazy list that captures the original list as well
   as the (possibly impure) mapping function.
 - Filtering a lazy list produces a lazy list that captures the original list as well
   as the (possibly impure) filtering predicate.
 - Concatenating two lazy lists produces a lazy list that captures both arguments.
 - Dropping elements from a lazy list gives a safe approximation where the original list is captured in the result. In fact, it's only some suffix of the list that is retained at run time, but our modeling identifies lazy lists and their suffixes, so this additional knowledge would not be useful.

Of course the function passed to `map` or `filter` could also be pure. After all, `A -> B` is a subtype of `(A -> B)^{any}` which is the same as `A => B`. In that case, the pure function
argument will _not_ show up in the result type of `map` or `filter`. For instance:
```scala sc:nocompile
val xs = squares(10)
val ys: LzyList[Int]^{xs} = xs.map(_ + 1)
```
The type of the mapped list `ys` has only `xs` in its capture set. The actual function
argument does not show up since it is pure. Likewise, if the lazy list
`xs` was pure, it would not show up in any of the method results.
This demonstrates that capability-based
effect systems with capture checking are naturally _effect polymorphic_.

This concludes our example. It's worth mentioning that an equivalent program defining and using standard, strict lists would require no capture annotations whatsoever. It would compile exactly as written now in standard Scala 3, yet one gets the capture checking for free. Essentially, `=>` already means "can capture anything" and since in a strict list side effecting operations are not retained in the result, there are no additional captures to record. A strict list could of course capture side-effecting closures in its elements but then tunneling applies, since
these elements are represented by a type variable. This means we don't need to annotate anything there either.

Another possibility would be a variant of lazy lists that requires all functions passed to `map`, `filter` and other operations like it to be pure. E.g. `map` on such a list would be defined like this:
```scala sc:nocompile
extension [A](xs: LzyList[A])
  def map[B](f: A -> B): LzyList[B] = ...
```
That variant would not require any capture annotations either.

To summarize, there are two "sweet spots" of data structure design: strict lists in
side-effecting or resource-aware code and lazy lists in purely functional code.
Both are already correctly capture-typed without requiring any explicit annotations. Capture annotations only come into play where the semantics gets more complicated because we deal with delayed effects such as in impure lazy lists or side-effecting iterators over strict lists. This property is probably one of the greatest plus points of our approach to capture checking compared to previous techniques which tend to be more noisy.
