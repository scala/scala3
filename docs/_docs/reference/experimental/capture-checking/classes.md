---
layout: doc-page
title: "Capture Checking of Classes"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/classes.html
---

## Introduction

The principles for capture checking closures also apply to classes. For instance, consider:
```scala
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
```scala
import annotation.constructorOnly

class NullLogger(using @constructorOnly fs: FileSystem):
  ...
def test2(using fs: FileSystem): NullLogger = NullLogger() // OK
```

The captured references of a class include _local capabilities_ and _argument capabilities_. Local capabilities are capabilities defined outside the class and referenced from its body. Argument capabilities are passed as parameters to the primary constructor of the class. Local capabilities are inherited:
the local capabilities of a superclass are also local capabilities of its subclasses. Example:

```scala
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

The capture set of the type of `this` of a class is inferred by the capture checker, unless the type is explicitly declared with a self type annotation like this one:
```scala
class C:
  self: D^{a, b} => ...
```
The inference observes the following constraints:

 - The type of `this` of a class `C` includes all captured references of `C`.
 - The type of `this` of a class `C` is a subtype of the type of `this`
   of each parent class of `C`.
 - The type of `this` must observe all constraints where `this` is used.

For instance, in
```scala
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

## Capture Tunnelling

Consider the following simple definition of a `Pair` class:
```scala
class Pair[+A, +B](x: A, y: B):
  def fst: A = x
  def snd: B = y
```
What happens if `Pair` is instantiated like this (assuming `ct` and `fs` are two capabilities in scope)?
```scala
def x: Int ->{ct} String
def y: Logger^{fs}
def p = Pair(x, y)
```
The last line will be typed as follows:
```scala
def p: Pair[Int ->{ct} String, Logger^{fs}] = Pair(x, y)
```
This might seem surprising. The `Pair(x, y)` value does capture capabilities `ct` and `fs`. Why don't they show up in its type at the outside?

The answer is capture tunnelling. Once a type variable is instantiated to a capturing type, the
capture is not propagated beyond this point. On the other hand, if the type variable is instantiated
again on access, the capture information "pops out" again. For instance, even though `p` is technically pure because its capture set is empty, writing `p.fst` would record a reference to the captured capability `ct`. So if this access was put in a closure, the capability would again form part of the outer capture set. E.g.
```scala
() => p.fst : () -> Int ->{ct} String
```
In other words, references to capabilities "tunnel through" in generic instantiations from creation to access; they do not affect the capture set of the enclosing generic data constructor applications.
This principle plays an important part in making capture checking concise and practical.


## A Larger Example

As a larger example, we present an implementation of lazy lists and some use cases. For simplicity,
our lists are lazy only in their tail part. This corresponds to what the Scala-2 type `Stream` did, whereas Scala 3's `LazyList` type computes strictly less since it is also lazy in the first argument.

Here is the base trait `LzyList` for our version of lazy lists:
```scala
trait LzyList[+A]:
  def isEmpty: Boolean
  def head: A
  def tail: LzyList[A]^{this}
```
Note that `tail` carries a capture annotation. It says that the tail of a lazy list can
potentially capture the same references as the lazy list as a whole.

The empty case of a `LzyList` is written as usual:
```scala
object LzyNil extends LzyList[Nothing]:
  def isEmpty = true
  def head = ???
  def tail = ???
```
Here is a formulation of the class for lazy cons nodes:
```scala
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
the private mutable field `cache`. Note that the typing of the assignment `cache = tl()` relies on the monotonicity rule for `{this}` capture sets.

Here is an extension method to define an infix cons operator `#:` for lazy lists. It is analogous
to `::` but instead of a strict list it produces a lazy list without evaluating its right operand.
```scala
extension [A](x: A)
  def #:(xs1: => LzyList[A]^): LzyList[A]^{xs1} =
    LzyCons(x, () => xs1)
```
Note that `#:` takes an impure call-by-name parameter `xs1` as its right argument. The result
of `#:` is a lazy list that captures that argument.

As an example usage of `#:`, here is a method `tabulate` that creates a lazy list
of given length with a generator function `gen`. The generator function is allowed
to have side effects.
```scala
def tabulate[A](n: Int)(gen: Int => A): LzyList[A]^{gen} =
  def recur(i: Int): LzyList[A]^{gen} =
    if i == n then LzyNil
    else gen(i) #: recur(i + 1)
  recur(0)
```
Here is a use of `tabulate`:
```scala
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
```scala
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
 - Dropping elements from a lazy list gives a safe approximation where the original list is captured in the result. In fact, it's only some suffix of the list that is retained at run time, but our modelling identifies lazy lists and their suffixes, so this additional knowledge would not be useful.

Of course the function passed to `map` or `filter` could also be pure. After all, `A -> B` is a subtype of `(A -> B)^{cap}` which is the same as `A => B`. In that case, the pure function
argument will _not_ show up in the result type of `map` or `filter`. For instance:
```scala
val xs = squares(10)
val ys: LzyList[Int]^{xs} = xs.map(_ + 1)
```
The type of the mapped list `ys` has only `xs` in its capture set. The actual function
argument does not show up since it is pure. Likewise, if the lazy list
`xs` was pure, it would not show up in any of the method results.
This demonstrates that capability-based
effect systems with capture checking are naturally _effect polymorphic_.

This concludes our example. It's worth mentioning that an equivalent program defining and using standard, strict lists would require no capture annotations whatsoever. It would compile exactly as written now in standard Scala 3, yet one gets the capture checking for free. Essentially, `=>` already means "can capture anything" and since in a strict list side effecting operations are not retained in the result, there are no additional captures to record. A strict list could of course capture side-effecting closures in its elements but then tunnelling applies, since
these elements are represented by a type variable. This means we don't need to annotate anything there either.

Another possibility would be a variant of lazy lists that requires all functions passed to `map`, `filter` and other operations like it to be pure. E.g. `map` on such a list would be defined like this:
```scala
extension [A](xs: LzyList[A])
  def map[B](f: A -> B): LzyList[B] = ...
```
That variant would not require any capture annotations either.

To summarize, there are two "sweet spots" of data structure design: strict lists in
side-effecting or resource-aware code and lazy lists in purely functional code.
Both are already correctly capture-typed without requiring any explicit annotations. Capture annotations only come into play where the semantics gets more complicated because we deal with delayed effects such as in impure lazy lists or side-effecting iterators over strict lists. This property is probably one of the greatest plus points of our approach to capture checking compared to previous techniques which tend to be more noisy.
