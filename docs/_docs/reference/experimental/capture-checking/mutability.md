---
layout: doc-page
title: "Mutability"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/mutability.html
---

## Introduction

An important class of effects represents accesses to mutable variables and mutable data structures. Here we distinguish two kinds of accesses: write access and read-only access. This is reflected by the kinds of capabilities permitting these effects.
A write capability to a mutable data structure `x` is just `x`. The corresponding read-only capability is written `x.rd`.

Mutable data structures are expressed with the marker trait `caps.Mutable`.
For instance, consider a simple reference cell:
```scala
import caps.Mutable

class Ref[T](init: T) extends Mutable:
  var fld: T

val r: Ref[Int]^ = Ref(22)
```
A function `() => r.fld += 1` has type `() ->{r} Unit` since it writes to the field `fld` of `r`. By contrast, a function `() => r.fld` has type `() ->{r.rd} Int` since it only reads the contents of `r` but does not update it.

## Capability Kinds

A capability is called
  - _shared_ if it is [classified](classifiers.md) as a `SharedCapability`
  - _exclusive_ otherwise.

## The Mutable Trait

We introduce a new trait
```scala
trait Mutable extends ExclusiveCapability, Classifier
```
It is used as a [classifier](classifiers.md) trait for types that define mutable variables and/or _update methods_.

## Update Methods

Update methods are declared using a new soft modifier `update`.

**Example:**
```scala
class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x
```
`update` can only be used in classes or objects extending `Mutable`. An update method is allowed to access exclusive capabilities in the method's environment. By contrast, a normal method in a type extending `Mutable` may access exclusive capabilities only if they are defined in the method itself or passed to it in parameters.

In class `Ref`, the `set` method should be declared as an update method since it accesses `this` as an exclusive write capability by writing to the variable `this.current` in its environment.

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

A type is called a _mutable_ if it extends `Mutable` and it has a mutable variable or
an update method or an update class as non-private member or constructor.

When we create an instance of a mutable type we always add `cap` to its capture set. For instance, if class `Ref` is declared as shown previously then `new Ref(1)` has type `Ref[Int]^`.

**Restriction:** A non-mutable type cannot be downcast by a pattern match to a mutable type. (Note: This is currently not enforced)

**Definition:** A parent class constructor is _read-only_ if the following conditions are met:

 1. The class does not retain any exclusive capabilities from its environment.
 2. The constructor does not take arguments that retain exclusive capabilities.
 3. The class does not does not have fields that retain exclusive universal capabilities.

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

A reference to a type extending trait `Mutable` gets an implicit capture set `{cap.rd}` provided no explicit capture set is given. This is different from other capability traits which implicitly add `{cap}`.

For instance, consider:
```scala
def addContents(from: Ref[Int], to: Ref[Int]^): Unit =
  to.set(to.get + from.get)
```
Here, `from` is implicitly read-only, and `to`'s type has capture set `cap`. I.e. with explicit capture sets this would read:
```scala
def addContents(from: Ref[Int]^{cap.rd}, to: Ref[Int]^{cap}): Unit
```
In other words, the explicit `^` indicates where write effects can happen.

## Read-Only Accesses

An access `p.m` to an update method or class `m` in a mutable type is permitted only if the type `M` of the prefix `p` retains exclusive capabilities. If `M` is pure or its capture set has only shared and read-only capabilities then the access is not permitted.

A _read-only access_ is a reference to a type extending `Mutable` where one of the following conditions holds:

 1. The reference is `this` and the access is not from an update method of the class of `this`. For instance:
    ```scala
    class Ref[T] extends Mutable:
      var current: T
      def get: T = this.current // read-only access to `this`
    ```
 2. The reference is a path where the path itself or a prefix of that path has a read-only capture set. For instance:
    ```scala
    val r: Ref[Int]^{cap.rd} = new Ref[T](22)
    def get = r.get // read-only access to `r`
    ```
    Another example:
    ```scala
    class RefContainer extends Mutable:
      val r: Ref[Int]^ = new Ref[Int](22)
    val c: RefContainer = RefContainer()
    def get = c.r.get // read-only access to `c.r`
    ```
    In the last example, `c.r` is a read-only access since the prefix `c` is a read-only reference. Note that `^{cap.rd}` was implicitly added to `c: RefContainer` since `RefContainer` is a `Mutable` class.
 3. The expected type of the reference is a value type that is not a mutable type. For instance:
    ```scala
    val r: Ref[Int]^ = Ref(22)
    val x: Object = r     // read-only access to `r`
    ```
 4. The reference is immediately followed by a selection with a member that is a normal method or class (not an update method or class). For instance:
    ```scala
    val r: Ref[Int]^ = Ref(22)
    r.get                 // read-only access to `r`
    ```

The first two conditions represent safety conditions: we _must_ declare the access a read-only access since the context of the access does not permit updates. The last two conditions are opportunistic: we _are allowed_ to declare the access a read-only access since the context
of the access does not require write capabilities.

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

## Update Restrictions

If a capability `r` is a read-only access, then one cannot use `r` to call an update method of `r` or to assign to a field of `r`. E.g. `r.set(22)` and `r.current = 22` are both disallowed.

Example:

```scala
class Ref[T](init: T) extends Mutable:
  var current = init

  update def set(x: T) =
    current = x   // ok, set is an update method

  def badSet(x: T) =
    current = x   // disallowed, since `this` is read-only access

val r: Ref[Int]^ = Ref(0)
r.set(22)         // ok, `r` is exclusive capability.

val ro: Ref[Int] = r
ro.set(22)        // disallowed, since `ro` is read-only access
```


## Transparent Vars

Sometimes, disallowing assignments to mutable fields from normal methods is too restrictive. For instance:
```scala
class Cache[T](eval: () -> T):
  private transparent var x: T = compiletime.uninitialized
  private transparent var known = false
  def force: T =
    if !known then
      x = eval()
      known = true
    x
```
Here, the mutable field `x` is used to store the result of a pure function `eval`. This is equivalent to just calling `eval()` directly but can be more efficient since the cached value is
evaluated at most once. So from a semantic standpoint, it should not be necessary to make `force` an update method, even though it does assign to `x`.

We can avoid the need for update methods by declaring mutable fields `transparent`. Assignments to `transparent` mutable field are not checked for read-only restrictions. It is up to the developer
to use `transparent` responsibly so that it does not hide visible side effects on mutable state.

Note that an assignment to a variable is restricted only if the variable is a field of a `Mutable` class. Fields of other classes and local variables are currently not checked.

It is planned to tighten the rules in the future so that non-transparent mutable fields can be declared only in  classes extending `Mutable`. This means that all assignments to mutable fields would be checked with the read-only restriction, and `transparent` would become essential as
an escape hatch.

By contrast, it is not planned to check assignments to local mutable variables, which are not fields of some class. So `transparent` is disallowed for such local variables.

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

