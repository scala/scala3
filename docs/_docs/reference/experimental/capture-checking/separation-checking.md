---
layout: doc-page
title: "Separation Checking"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/separation-checking.html
---

## Introduction

Separation checking is an extension of capture checking that enforces unique, un-aliased access to capabilities. It is enabled by a language import
```scala
import language.experimental.separationChecking
```
(or the corresponding setting `-language:experimental.separationChecking`).
The import has to be given in addition to the `language.experimental.captureChecking` import that enables capture checking.
The reason for the second language import is that separation checking is less mature than capture checking proper, so we are less sure
we got the balance of safety and expressivity right for it at the present time.

The purpose of separation checking is to ensure that certain accesses to capabilities are not aliased. As an example, consider matrix multiplication. A method that multiplies matrices `a`, and `b` into `c`,  could be declared like this:
```scala
def multiply(a: Matrix, b: Matrix, c: Matrix): Unit
```
But that signature alone does not tell us which matrices are supposed to be inputs and which one is the output. Nor does it guarantee that an input matrix is not re-used as the output, which would lead to wrong results.

With separation checking, we can declare `Matrix` as a `Mutable` type, like this:
```scala
class Matrix(nrows: Int, ncols: Int) extends Mutable:
  update def setElem(i: Int, j: Int, x: Double): Unit = ...
  def getElem(i: Int, j: Int): Double = ...
```
We further declare that the `setElem` method in a `Matrix` is an `update` method, which means it has a side effect.

Separation checking gives a special interpretation to the following modified signature of `multiply`:
```scala
def multiply(a: Matrix, b: Matrix, c: Matrix^): Unit
```
In fact, only a single character was added: `c`'s type now carries a universal capability. This signature enforces at the same time two desirable properties:

 - Matrices `a`, and `b` are read-only; `multiply` will not call their update method. By contrast, the `c` matrix can be updated.
 - Matrices `a` and `b` are different from matrix `c`, but `a` and `b` could refer to the same matrix.

So, effectively, anything that can be updated must be unaliased.



## Capability Kinds

A capability is called
  - _shared_ if it is [classified](classifiers.md) as a `SharedCapability`
  - _exclusive_ otherwise.

## The Mutable Trait

We introduce a new trait
```scala
trait Mutable extends ExclusiveCapability, Classifier
```
It is used as a [classifier](classifiers.md) trait for types that define _update methods_ using
a new soft modifier `update`.

**Example:**
```scala
class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x
```
`update` can only be used in classes or objects extending `Mutable`. An update method is allowed to access exclusive capabilities in the method's environment. By contrast, a normal method in a type extending `Mutable` may access exclusive capabilities only if they are defined locally or passed to it in parameters.

In class `Ref`, the `set` should be declared as an update method since it accesses the exclusive write capability of the variable `current` in its environment.

`update` can also be used on an inner class of a class or object extending `Mutable`. It gives all code in the class the right
to  access exclusive capabilities in the class environment. Normal classes
can only access exclusive capabilities defined in the class or passed to it in parameters.

```scala
object Registry extends Mutable:
  var count = 0
  update class Counter:
    update def next: Int =
      count += 1
      count
```
Normal method members of `Mutable` classes cannot call update methods. This is indicated since accesses in the callee are recorded in the caller. So if the callee captures exclusive capabilities so does the caller.

An update method cannot implement or override a normal method, whereas normal methods may implement or override update methods. Since methods such as `toString` or `==` inherited from Object are normal methods, it follows that none of these methods may be implemented as an update method.

The `apply` method of a function type is also a normal method, hence `Mutable` classes may not implement a function type using an update method as the `apply` method.

## Mutable Types

A type is called a _mutable_ if it extends `Mutable` and it has an update method or an update class as non-private member or constructor.

When we create an instance of a mutable type we always add `cap` to its capture set. For instance, if class `Ref` is declared as shown previously then `new Ref(1)` has type `Ref[Int]^`.

**Restriction:** A non-mutable type cannot be downcast by a pattern match to a mutable type.

**Definition:** A class is _read_only_ if the following conditions are met:

 1. It does not extend any exclusive capabilities from its environment.
 2. It does not take parameters with exclusive capabilities.
 3. It does not contain mutable fields, or fields that take exclusive capabilities.

**Restriction:** If a class or trait extends `Mutable` all its parent classes or traits must either extend `Mutable` or be read-only.

The idea is that when we upcast a reference to a type extending `Mutable` to a type that does not extend `Mutable`, we cannot possibly call a method on this reference that uses an exclusive capability. Indeed, by the previous restriction this class must be a read-only class, which means that none of the code implemented
in the class can access exclusive capabilities on its own. And we
also cannot override any of the methods of this class with a method
accessing exclusive capabilities, since such a method would have
to be an update method and update methods are not allowed to override regular methods.



**Example:**

Consider trait `IterableOnce` from the standard library.

```scala
trait IterableOnce[+T] extends Mutable:
  def iterator: Iterator[T]^{this}
  update def foreach(op: T => Unit): Unit
  update def exists(op: T => Boolean): Boolean
  ...
```
The trait is a mutable type with many update methods, among them `foreach` and `exists`. These need to be classified as `update` because their implementation in the subtrait `Iterator` uses the update method `next`.
```scala
trait Iterator[T] extends IterableOnce[T]:
  def iterator = this
  def hasNext: Boolean
  update def next(): T
  update def foreach(op: T => Unit): Unit = ...
  update def exists(op; T => Boolean): Boolean = ...
  ...
```
But there are other implementations of `IterableOnce` that are not mutable types (even though they do indirectly extend the `Mutable` trait). Notably, collection classes implement `IterableOnce` by creating a fresh
`iterator` each time one is required. The mutation via `next()` is then restricted to the state of that iterator, whereas the underlying collection is unaffected. These implementations would implement each `update` method in `IterableOnce` by a normal method without the `update` modifier.

```scala
trait Iterable[T] extends IterableOnce[T]:
  def iterator = new Iterator[T] { ... }
  def foreach(op: T => Unit) = iterator.foreach(op)
  def exists(op: T => Boolean) = iterator.exists(op)
```
Here, `Iterable` is not a mutable type since it has no update method as member.
All inherited update methods are (re-)implemented by normal methods.

**Note:** One might think that we don't need a base trait `Mutable` since in any case
a mutable type is defined by the presence of update methods, not by what it extends. In fact the importance of `Mutable` is that it defines _the other methods_ as read-only methods that _cannot_ access exclusive capabilities. For types not extending `Mutable`, this is not the case. For instance, the `apply` method of a function type is not an update method and the type itself does not extend `Mutable`. But `apply` may well be implemented by
a method that accesses exclusive capabilities.

## Read-only Capabilities

If `x` is an exclusive capability of a type extending `Mutable`, `x.rd` is its associated _read-only_ capability. It counts as a shared capability. A read-only capability does not permit access to the mutable fields of a matrix.

A read-only capability can be seen as a classified capability
using a classifier trait `Read` that extends `Mutable`. I.e.
`x.rd` can be seen as being essentially the same as `x.only[Read]`.
(Currently, this precise equivalence is still waiting to be implemented.)

**Implicitly added capture sets**

A reference to a type extending trait `Mutable` gets an implicit capture set `{cap.rd}` provided no explicit capture set is given. This is different from
other capability traits which implicitly add `{cap}`.

For instance, consider the matrix multiplication method mentioned previously:
```scala
def multiply(a: Matrix, b: Matrix, c: Matrix^): Unit
```
Here, `a` and `b` are implicitly read-only, and `c`'s type has capture set `cap`. I.e. with explicit capture sets this would read:
```scala
def mul(a: Matrix^{cap.rd}, b: Matrix^{cap.rd}, c: Matrix^{cap}): Unit
```
Separation checking will then make sure that `a` and `b` must be different from `c`.

## Accesses to Mutable Types

An access `p.m` to an update method or class `m` in a mutable type is permitted only if the type `M` of the prefix `p` retains exclusive capabilities. If `M` is pure or its capture set has only shared
capabilities then the access is not permitted.

A _read-only access_ is a reference `x` to a type extending `Mutable` that is either

 - widened to a value type that is not a mutable type, or
 - immediately followed by a selection with a member that is a normal method or class (not an update method or class).

A read-only access charges the read-only capability `x.rd` to its environment. Other accesses charge the full capability `x`.

**Example:**

Consider a reference `x` and two closures `f` and `g`.

```scala
val x = Ref(1)
val f = () => x.get    // f: () ->{x.rd} Unit
val g = () => x.set(1) // g: () ->{x} Unit
```

`f` accesses a regular method, so it charges only `x.rd` to its environment which shows up in its capture set. By contrast, `g`
accesses an update method of `x`, so its capture set is `{x}`.

A reference to a mutable type with an exclusive capture set can be widened to a reference with a read-only set. For instance, the following is OK:
```scala
val a: Ref^ = Ref(1)
val b1: Ref^{a.rd} = a
val b2: Ref^{cap.rd} = a
```

## Read-Only Capsets

If we consider subtyping and subcapturing, we observe what looks like a contradiction: `x.rd` is seen as a restricted capability, so `{x.rd}` should subcapture `{x}`. Yet, we have seen in the example above that sometimes it goes the other way: `a`'s capture set is either `{a}` or `{cap}`, yet `a` can be used to define `b1` and `b2`, with capture sets `{a.rd}` and `{cap.rd}`, respectively.

The contradiction can be explained by noting that we use a capture set in two different roles.

First, and as always, a capture set defines _retained capabilities_ that may or may be not used by a value. More capabilities give larger types, and the empty capture set is the smallest set according to that ordering. That makes sense: If a higher-order function like `map` is willing to accept a function `A => B` that can have arbitrary effects it's certainly OK to pass a pure function of type `A -> B` to it.

But for mutations, we use a capture set in a second role, in which it defines a set of _access permissions_. If we have a `Ref^`, we can access all its methods, but if we have a `Ref^{cap.rd}`, we can access only regular methods, not update methods. From that viewpoint a mutable type with exclusive capabilities lets you do more than a mutable type with just read-only capabilities. So by the Liskov substitution principle, sets with exclusive capabilities subcapture sets with only read-only capabilities.

The contradiction can be solved by distinguishing these two roles. For access permissions, we express read-only sets with an additional _qualifier_ `RD`. That qualifier is used only in the formal theory and the implementation, it currently cannot be expressed in source.
We add an implicit read-only qualifier `RD` to all capture sets on mutable types that consist only of shared or read-only capabilities.
So when we write
```scala
val b1: Ref^{a.rd} = a
```
we really mean
```scala
val b1: Ref^{a.rd}.RD = a
```

The subcapturing theory for sets is then as before, with the following additional rules:

 - `C <: C.RD`
 - `C₁.RD <: C₂.RD` if `C₍ <: C₂`
 - `{x, ...}.RD = {x.rd, ...}.RD`
 - `{x.rd, ...} <: {x, ...}`

## Separation Checking

The idea behind separation checking is simple: We now interpret each occurrence of `cap` as a separate top capability. This includes derived syntaxes like `A^` and `B => C`. We further keep track during capture checking which capabilities are subsumed by each `cap`. If capture checking widens a capability `x` to a top capability `capᵢ`, we say `x` is _hidden_ by `capᵢ`. The rule then is that any capability hidden by a top capability `capᵢ` cannot be referenced independently or hidden in another `capⱼ` in code that can see `capᵢ`.

Here's an example:
```scala
val x: C^ = y
  ... x ...  // ok
  ... y ...  // error
```
This principle ensures that capabilities such as `x` that have `cap` as underlying capture set are un-aliased or "fresh". Any previously existing aliases such as `y` in the code above are inaccessible as long as `x` is also visible.

Separation checking applies only to exclusive capabilities and their read-only versions. Any capability extending `SharedCapability` in its type is exempted; the following definitions and rules do not apply to them.

**Definitions:**

 - The _transitive capture set_ `tcs(c)` of a capability `c` with underlying capture set `C` is `c` itself, plus the transitive capture set of `C`.

 - The _transitive capture set_ `tcs(C)` of a capture set C is the union
   of `tcs(c)` for all elements `c` of `C`.

 - Two capture sets _interfere_ if one contains an exclusive capability `x` and the other also contains `x` or contains the read-only capability `x.rd`. Conversely, two capture sets are _separated_ if their transitive capture sets don't interfere.

Separation checks are applied in the following scenarios:

### Checking Applications

When checking a function application `f(e_1, ..., e_n)`, we instantiate each `cap` in a formal parameter of `f` to a fresh top capability and compare the argument types with these instantiated parameter types. We then check that the hidden set of each instantiated top capability for an argument `eᵢ` is separated from the capture sets of all the other arguments as well as from the capture sets of the function prefix and the function result. For instance a
call to
```scala
multiply(a, b, a)
```
would be rejected since `a` appears in the hidden set of the last parameter of multiply, which has type `Matrix^` and also appears in the capture set of the
first parameter.

We do not report a separation error between two sets if a formal parameter's capture set explicitly names a conflicting parameter. For instance, consider a method `seq` to apply two effectful function arguments in sequence. It can be declared as follows:
```scala
def seq(f: () => Unit; g: () ->{cap, f} Unit): Unit =
  f(); g()
```
Here, the `g` parameter explicitly mentions `f` in its potential capture set. This means that the `cap` in the same capture set would not need to hide the  first argument, since it already appears explicitly in the same set. Consequently, we can pass the same function twice to `compose` without violating the separation criteria:
```scala
val r = Ref(1)
val plusOne = r.set(r.get + 1)
seq(plusOne, plusOne)
```
Without the explicit mention of parameter `f` in the capture set of parameter `g` of `seq` we'd get a separation error, since the transitive capture sets of both arguments contain `r` and are therefore not separated.

### Checking Statement Sequences

When a capability `x` is used at some point in a statement sequence, we check that `{x}` is separated from the hidden sets of all previous definitions.

Example:
```scala
val a: Ref^ = Ref(1)
val b: Ref^ = a
val x = a.get // error
```
Here, the last line violates the separation criterion since it uses in `a.get` the capability `a`, which is hidden by the definition of `b`.
Note that this check only applies when there are explicit top capabilities in play. One could very well write
```scala
val a: Ref^ = Ref(1)
val b: Ref^{a} = a
val x = a.get // ok
```
One can also drop the explicit type of `b` and leave it to be inferred. That would
not cause a separation error either.
```scala
val a: Ref^ = Ref(0
val b = a
val x = a.get // ok
```

### Checking Types

When a type contains top capabilities we check that their hidden sets don't interfere with other parts of the same type.

Example:
```scala
val b: (Ref^, Ref^) = (a, a)       // error
val c: (Ref^, Ref^{a}) = (a, a)    // error
val d: (Ref^{a}, Ref^{a}) = (a, a) // ok
```
Here, the definition of `b` is in error since the hidden sets of the two `^`s in its type both contain `a`. Likewise, the definition of `c` is in error since the hidden set of the `^` in its type contains `a`, which is also part of a capture set somewhere else in the type. On the other hand, the definition of `d` is legal since there are no hidden sets to check.

### Checking Return Types

When a `cap` appears in the return type of a function it means a "fresh" top capability, different from what is known at the call site. Separation checking makes sure this is the case. For instance, the following is OK:
```scala
def newRef(): Ref^ = Ref(1)
```
And so is this:
```scala
def newRef(): Ref^ =
  val a = Ref(1)
  a
```
But the next definitions would cause a separation error:
```scala
val a = Ref(1)
def newRef(): Ref^ = a // error
```
The rule is that the hidden set of a fresh cap in a return type cannot reference exclusive or read-only capabilities defined outside of the function. What about parameters? Here's another illegal version:
```scala
def incr(a: Ref^): Ref^ =
  a.set(a.get + 1)
  a
```
These needs to be rejected because otherwise we could have set up the following bad example:
```scala
val a = Ref(1)
val b: Ref^ = incr(a)
```
Here, `b` aliases `a` but does not hide it. If we referred to `a` afterwards we would be surprised to see that the reference has now a value of 2.
Therefore, parameters cannot appear in the hidden sets of fresh result caps either, at least not in general. An exception to this rule is described in the next section.

### Consume Parameters

Returning parameters in fresh result caps is safe if the actual argument to the parameter is not used afterwards. We can signal and enforce this pattern by adding a `consume` modifier to a parameter. With that new soft modifier, the following variant of `incr` is legal:
```scala
def incr(consume a: Ref^): Ref^ =
  a.set(a.get + 1)
  a
```
Here, we increment the value of a reference and then return the same reference while enforcing the condition that the original reference cannot be used afterwards. Then the following is legal:
```scala
val a1 = Ref(1)
val a2 = incr(a1)
val a3 = incr(a2)
println(a3)
```
Each reference `aᵢ` is unused after it is passed to `incr`. But the following continuation of that sequence would be in error:
```scala
val a4 = println(a2) // error
val a5 = incr(a1)    // error
```
In both of these assignments we use a capability that was consumed in an argument
of a previous application.

Consume parameters enforce linear access to resources. This can be very useful. As an example, consider Scala's  buffers such as `ListBuffer` or `ArrayBuffer`. We can treat these buffers as if they were purely functional, if we can enforce linear access.

For instance, we can define a function `linearAdd` that adds elements to buffers in-place without violating referential transparency:
```scala
def linearAdd[T](consume buf: Buffer[T]^, elem: T): Buffer[T]^ =
  buf += elem
```
`linearAdd` returns a fresh buffer resulting from appending `elem` to `buf`. It overwrites `buf`, but that's OK since the `consume` modifier on `buf` ensures that the argument is not used after the call.

### Consume Methods

Buffers in Scala's standard library use a single-argument method `+=` instead of a two argument global function like `linearAdd`. We can enforce linearity in this case by adding the `consume` modifier to the method itself.
```scala
class Buffer[T] extends Mutable:
  consume def +=(x: T): Buffer[T]^ = this // ok
```
`consume` on a method implies `update`, so there's no need to label `+=` separately as an update method. Then we can write
```scala
val b = Buffer[Int]() += 1 += 2
val c = b += 3
// b cannot be used from here
```
This code is equivalent to functional append with `+`, and is at the same time more efficient since it re-uses the storage of the argument buffer.

