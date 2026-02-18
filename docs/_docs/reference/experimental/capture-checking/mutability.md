---
layout: doc-page
title: "Stateful Capabilities"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/mutability.html
---

## Introduction

An important class of effects represents accesses to mutable variables and mutable data structures. This is intimately tied with the concept of _program state_. Stateful capabilities are capabilities that allow to consult and change the program state.

We distinguish two kinds of accesses: full access that allows state changes and read-only access that allows to observe the state but not to change it. This is reflected by the kinds of capabilities permitting these effects. A regular capability representing a stateful object `x` is just `x`. The corresponding read-only capability is written `x.rd`.

A common kind of stateful capabilities represent mutable variables that can be read and written.
These mutable data structures are expressed with the marker trait `caps.Mutable`.
For instance, consider a simple reference cell:
```scala
import caps.Mutable

class Ref[T](init: T) extends Mutable:
  var fld: T = init

val r: Ref[Int]^ = Ref(22)
```
A function `() => r.fld += 1` has type `() ->{r} Unit` since it writes to the field `fld` of `r`. By contrast, a function `() => r.fld` has type `() ->{r.rd} Int` since it only reads the contents of `r` but does not update it.

In fact, `Mutable` instances combine several properties that each are represented as separate traits.
In the following, we present these traits and their associated capabilities.

## Capability Kinds

A capability is called
  - _shared_ if it is [classified](classifiers.md) as a `SharedCapability`
  - _exclusive_ otherwise.

## The `Stateful` Trait

In the `scala.caps` object we define a new trait
```scala sc:nocompile
trait Stateful extends ExclusiveCapability
```
It is used as a marker trait for classes that can consult and change the global program state.
These classes typically contain mutable variables and/or _update methods_.

## Update Methods

Update methods are declared using a new soft modifier `update`.

**Example:**
```scala
//{
import caps.*
//}
class Counter extends Stateful:
  private var count = 0
  def value: Int = count
  update def incr(x: Int): Unit = count = x
```

`update` can only be used in classes or objects extending `Stateful`. An update method is allowed to access exclusive capabilities in the method's environment. By contrast, a normal method in a type extending `Stateful` may access exclusive capabilities only if they are defined in the method itself or passed to it in parameters.

In class `Counter`, the `incr` method should be declared as an update method since it accesses `this` as an exclusive write capability by writing to the variable `this.count` in its environment.

`update` can also be used on an inner class of a class or object extending `Stateful`. It gives all code in the inner class the right to  access exclusive capabilities in the outer class environment. Normal classes
can only access exclusive capabilities defined in the class or passed to it in parameters.

For instance, we can also define counter objects that update a shared variable that is external to the object:

```scala sc:nocompile
object Registry extends Stateful:
  var sharedCount = 0
  update class CounterX:
    update def next: Int =
      sharedCount += 1
      sharedCount
```

Normal method members of `Stateful` classes cannot call update methods. This is indicated since accesses in the callee are recorded in the caller. So if the callee captures exclusive capabilities so does the caller.

An update method cannot implement or override a normal method, whereas normal methods may implement or override update methods. Since methods such as `toString` or `==` inherited from Object are normal methods, it follows that none of these methods may be implemented as an update method.

The `apply` method of a function type is also a normal method, hence `Stateful` classes may not implement a function type using an update method as the `apply` method.

## Stateful Types

A _type_ is called a _stateful_ if it extends `Stateful` and it has a mutable variable or
an update method or an update class as non-private member or constructor.


**Restriction:** A non-stateful type cannot be downcast by a pattern match to a stateful type. (Note: This is currently not enforced)

**Definition:** A parent class constructor is _read-only_ if the following conditions are met:

 1. The class does not retain any exclusive capabilities from its environment.
 2. The constructor does not take arguments that retain exclusive capabilities.
 3. The class does not have fields that retain exclusive universal capabilities.

**Restriction:** If a class or trait extends `Stateful` all its parent classes or traits must either extend `Stateful` or be read-only.

The idea is that when we upcast a reference to a type extending `Stateful` to a type that does not extend `Stateful`, we cannot possibly call a method on this reference that uses an exclusive capability. Indeed, by the previous restriction this class must be a read-only class, which means that none of the code implemented
in the class can access exclusive capabilities on its own. And we
also cannot override any of the methods of this class with a method
accessing exclusive capabilities, since such a method would have
to be an update method and update methods are not allowed to override regular methods.

**Example:**

Consider the trait `IterableOnce` from the standard library.

```scala sc:nocompile
trait IterableOnce[+T] extends Stateful:
  def iterator: Iterator[T]^{this}
  update def foreach(op: T => Unit): Unit
  update def exists(op: T => Boolean): Boolean
  ...
```

The trait is a stateful type with many update methods, among them `foreach` and `exists`. These need to be classified as `update` because their implementation in the subtrait `Iterator` uses the update method `next`.
```scala sc:nocompile
trait Iterator[T] extends IterableOnce[T]:
  def iterator = this
  def hasNext: Boolean
  update def next(): T
  update def foreach(op: T => Unit): Unit = ...
  update def exists(op: T => Boolean): Boolean = ...
  ...
```
But there are other implementations of `IterableOnce` that are not stateful types (even though they do indirectly extend the `Stateful` trait). Notably, collection classes implement `IterableOnce` by creating a fresh
`iterator` each time one is required. The mutation via `next()` is then restricted to the state of that iterator, whereas the underlying collection is unaffected. These implementations would implement each `update` method in `IterableOnce` by a normal method without the `update` modifier.

```scala sc:nocompile
trait Iterable[T] extends IterableOnce[T]:
  def iterator = new Iterator[T] { ... }
  def foreach(op: T => Unit) = iterator.foreach(op)
  def exists(op: T => Boolean) = iterator.exists(op)
```
Here, `Iterable` is not a stateful type since it has no update method as member.
All inherited update methods are (re-)implemented by normal methods.

**Note:** One might think that we don't need a base trait `Stateful` since in any case
a stateful type is defined by the presence of update methods, not by what it extends. In fact the importance of `Stateful` is that it defines _the other methods_ as read-only methods that _cannot_ access exclusive capabilities. For types not extending `Stateful`, this is not the case. For instance, the `apply` method of a function type is not an update method and the type itself does not extend `Stateful`. But `apply` may well be implemented by a method that accesses exclusive capabilities.

A mutable class such as a reference cell or a mutable array or matrix is clearly a stateful type.
But it also has two other properties that are explained in the following.

## Separate Classes

Each time one creates a value of a mutable type one gets a separate fresh object that can be updated independently
of other objects. This property is expressed by extending the `Separate` trait in the `scala.caps` object:
```scala sc:nocompile
trait Separate extends Stateful
```
If a value of a type extending Separate is created, a fresh `any` is automatically
added to the value's capture set:
```scala sc:nocompile
class S extends Separate
val s = S()   // s: S^
```

Whether or not a class should be `Separate` is a design question. For instance here is a
design of `Iterator` with a `map` method that is `Stateful` but not `Separate`:

```scala sc:nocompile
class Iterator[T] extends Stateful:
  def hasNext: Boolean
  update def next: T
  update def map[U](f: T => U): Iterator[U]^{this, f} = new Iterator:
    def hasNext = Iterator.this.hasNext
    update def next = Iterator.this.next

def listIterator[T](xs: List[T]): Iterator[T]^ = new Iterator:
  var current = xs
  def hasNext = current.nonEmpty
  def next = try current.head finally current = current.tail
```

Here, `listIterator` returns a fresh iterator with separate state, whereas `map` returns an iterator capturing the current iterator `this` and the passed function `f`, without representing a separate state.

One could also decide to make iterator a `Separate` class:

```scala sc:nocompile
class SepIterator[T] extends Stateful, Separate:
  def hasNext: Boolean
  update def next: T
  consume def map[U](consume f: T => U) = new SepIterator:
    def hasNext = Iterator.this.hasNext
    update def next: Iterator.this.next
```
`SepIterator`'s `map` method returns a fresh iterator of type `SepIterator[U]^`. We lose the knowledge that this iterator captures `this` and `f`. So this second version of iterators might seem less useful than the first. However, we can check aliasing conditions using [separation checking](./separation-checking.md). Separation checking
would inform us that creating a mapped iterator invalidates any future accesses to the original iterator or the passed function `f`. This is expressed by the `consume` modifiers on the `map` method and it parameter.  The `consume` modifier will be explained in detail in the section on separation checking.

## The Unscoped Classifier

Usually, capabilities have bounded lifetimes. For instance, consider again the withFile method:
```scala sc:nocompile
class File:
  def read(): String = ...
  def close(): Unit = ...

def withFile[T](op: (f: File^) => T): T =
  op(new File)
```
Here, we need to enforce that the return type of `op` cannot possibly capture the
`File` parameter `f`. This is achieved by preventing `op` from returning new capabilities
that are not already known outside the call to `withFile`. But this scheme can be too restrictive.
For instance, we might want to read the file's contents into a `Separate` capability such as
a `Ref` cell. That `Ref` cell would not hold on to the file, but it would not be a pure type either,
since the cell itself is a capability.

We can make this compile by declaring `Ref` cells to be `Unscoped`.
Capabilities classified as `Unscoped` can escape their environment. For instance, the following
is permitted:
```scala sc:nocompile
class Ref[T](init: T) extends Separate, Unscoped

withFile: f =>
  val r: Ref[String]^ = Ref(f.read())
  r
```
Here, `r` is a fresh reference of type `Ref[String]` that escapes the scope of `withFile`. That's OK only since
`Ref` is classified as `Unscoped`. Since `Unscoped` is a [classifier](./classifiers.md) it means that `Ref` cannot possibly capture `f`, which as a `File` is not classified as `Unscoped`. So returning a `Ref`
from a `withFile` does not affect the lifetime of `f`.

## Mutable Classes

Classes such as ref-cells, arrays, or matrices are stateful, unscoped, and their instances represent fresh capabilities. This common combination is expressed by the `Mutable` trait in the `scala.caps` object.

```scala sc:nocompile
trait Mutable extends Stateful, Separate, Unscoped
```

Examples:

```scala sc:nocompile
class Ref[T](init: T) extends Mutable:
  private var current = init
  def get: T = current
  update def set(x: T): Unit = current = x

class Arr[T](n: Int) extends Mutable:
  private val elems = new Array[T](n)
  def apply(i: Int): T = elems(i)
  update def update(i: Int, x: T) = elems(i) = x
```

An example of a `Stateful` and `Unscoped` capability that is _not_ `Separate` would be a
facade class that reveals some part of an underlying `Mutable` capability.

## Arrays

The class `scala.Array` is considered a `Mutable` class if [separation checking](./separation-checking.md) is enabled. In that context, class Array can be considered to be declared roughly as follows:
```scala sc:nocompile
class Array[T] extends Mutable:
  def length: Int
  def apply(i: Int): T
  update def update(i: Int, x: T): Unit
```
In fact, for technical reasons `Array` cannot extend `Mutable` or any other new traits beyond what is supported by the JVM. But for the purposes of capture and separation checking, it is still a considered a `Mutable` class.

By contrast, none of the mutable collections in the Scala standard library extend currently `Stateful` or `Mutable`. So to experiment with mutable collections, an
alternative class library has to be used.

## Read-only Capabilities

If `x` is an exclusive capability of a type extending `Stateful`, `x.rd` is its associated _read-only_ capability.

**Implicitly added capture sets**

A reference to a type extending trait `Stateful` gets an implicit capture set `{any.rd}` provided no explicit capture set is given. This is different from other capability traits which implicitly add `{any}`.

For instance, consider:
```scala sc:nocompile
def addContents(from: Ref[Int], to: Ref[Int]^): Unit =
  to.set(to.get + from.get)
```
Here, `from` is implicitly read-only, and `to`'s type has capture set `any`. I.e. with explicit capture sets this would read:
```scala sc:nocompile
def addContents(from: Ref[Int]^{any.rd}, to: Ref[Int]^{any}): Unit
```
In other words, the explicit `^` indicates where state changes can happen.

## Read-Only Accesses

An access `p.m` to an update method or class `m` in a stateful type is permitted only if the type `S` of the prefix `p` retains exclusive capabilities. If `S` is pure or its capture set has only shared and read-only capabilities then the access is not permitted.

A _read-only access_ is a reference to a type extending `Stateful` where one of the following conditions hold:

 1. The reference is `this` and the access is not from an update method of the class of `this`. For instance:
    ```scala sc:nocompile
    class Ref[T] extends Mutable:
      var current: T
      def get: T = this.current // read-only access to `this`
    ```
 2. The reference is a path where the path itself or a prefix of that path has a read-only capture set. For instance:
    ```scala sc:nocompile
    val r: Ref[Int]^{any.rd} = new Ref[T](22)
    def get = r.get // read-only access to `r`
    ```
    Another example:
    ```scala sc:nocompile
    class RefContainer extends Mutable:
      val r: Ref[Int]^ = new Ref[Int](22)
    val c: RefContainer = RefContainer()
    def get = c.r.get // read-only access to `c.r`
    ```
    In the last example, `c.r` is a read-only access since the prefix `c` is a read-only reference. Note that `^{any.rd}` was implicitly added to `c: RefContainer` since `RefContainer` is a `Stateful` capability class.
 3. The expected type of the reference is a value type that is not a stateful type. For instance:
    ```scala sc:nocompile
    val r: Ref[Int]^ = Ref(22)
    val x: Object = r     // read-only access to `r`
    ```
 4. The reference is immediately followed by a selection with a member that is a normal method or class (not an update method or class). For instance:
    ```scala sc:nocompile
    val r: Ref[Int]^ = Ref(22)
    r.get                 // read-only access to `r`
    ```

The first two conditions represent safety conditions: we _must_ declare the access a read-only access since the context of the access does not permit updates. The last two conditions are opportunistic: we _are allowed_ to declare the access a read-only access since the context
of the access does not require full capabilities.

A read-only access charges the read-only capability `x.rd` to its environment. Other accesses charge the full capability `x`.

**Example:**

Consider a reference `x` and two closures `f` and `g`.

```scala sc:nocompile
val x = Ref(1)
val f = () => x.get    // f: () ->{x.rd} Unit
val g = () => x.set(1) // g: () ->{x} Unit
```

`f` accesses a regular method, so it charges only `x.rd` to its environment which shows up in its capture set. By contrast, `g`
accesses an update method of `x`, so its capture set is `{x}`.

A reference to a stateful type with an exclusive capture set can be widened to a reference with a read-only set. For instance, the following is OK:
```scala sc:nocompile
val a: Ref[Int]^ = Ref(1)
val b1: Ref[Int]^{a.rd} = a
val b2: Ref[Int]^{any.rd} = a
```

## Lazy Vals and Read-Only Restrictions

Lazy val initializers in `Stateful` classes are subject to read-only restrictions similar to those for normal methods. Specifically, a lazy val initializer in a `Stateful` class cannot call update methods or refer to non-local exclusive capabilities, i.e., capabilities defined outside the lazy val's scope.

For example, when a lazy val is declared in a local method's scope, its initializer may freely use capabilities from the surrounding environment:
```scala sc:nocompile
def example(r: Ref[Int]^) =
  lazy val goodInit: () ->{r.rd} Int =
    val i = r.get() // ok: read-only access
    r.set(100 * i)  // ok: can call update method
    () => r.get() + i
```
However, within a `Stateful` class, a lazy val declaration has only read access to non-local exclusive capabilities:
```scala sc:nocompile
class Wrapper(val r: Ref[Int]^) extends Stateful:
  lazy val badInit: () ->{r} Int =
    r.set(100) // error: call to update method
    () => r.set(r.get() * 2); r.get() // error: call to update method

  lazy val goodInit: () ->{r.rd} Int =
    val i = r.get()   // ok
    () => r.get() * i // ok
```
The initializer of `badInit` attempts to call `r.set(100)`, an update method on the non-local exclusive capability `r`.
This is rejected because initializers should not perform mutations on external state.

### Local Capabilities

The restriction applies only to **non-local** capabilities. A lazy val can freely call update methods on capabilities it creates locally within its initializer:

```scala sc:nocompile
class Example:
  lazy val localMutation: () => Int =
    val local: Ref[Int]^ = Ref(10)  // created in initializer
    local.set(100)             // ok: local capability
    () => local.get()
```

Here, `local` is created within the lazy val's initializer, so it counts as a local capability. The initializer can call update methods on it.

This makes lazy vals behave like normal methods in `Stateful` classes: they can read from their environment but cannot update it unless explicitly marked.
Unlike for methods, there's currently no `update` modifier for lazy vals in `Stateful` classes, so their initialization is always read-only with respect to non-local capabilities. A future version of capture checking might
support `update lazy val` if there are compelling use cases and there is sufficient community demand.

## Update Restrictions

If a capability `r` is a read-only access, then one cannot use `r` to call an update method of `r` or to assign to a field of `r`. E.g. `r.set(22)` and `r.current = 22` are both disallowed.

Example:

```scala sc:nocompile
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

## Untracked Vars

Under [separation checking](./separation-checking.md), mutable fields are allowed to be declared only in `Stateful` classes. Updates to these fields can then only happen in update methods of these classes.

But sometimes, disallowing assignments to mutable fields from normal methods is too restrictive. For instance:
```scala sc:nocompile
import caps.unsafe.untrackedCaptures

class Cache[T](eval: () -> T):
  @untrackedCaptures private var x: T = compiletime.uninitialized
  @untrackedCaptures private var known = false
  def force: T =
    if !known then
      x = eval()
      known = true
    x
```
Note that `Cache` is not declared as a `Stateful` class, even though it has mutable fields. In this case, the mutable field `x` is used to store the result of a pure function `eval` and field `known` reflects whether `eval` was called. This is equivalent to just calling `eval()` directly but can be more efficient since the cached value is evaluated at most once. So from a semantic standpoint, it should not be necessary to make `Cache` a `Stateful` class with `force` as an update method, even though `force` does assign to `x`.

We can avoid the need for stateful classes and update methods by annotating mutable fields with `@untrackedCaptures`. Assignments to untracked mutable fields are then not checked for read-only restrictions. The `@untrackedCaptures` annotation can be imported from the `scala.caps.unsafe` object. It is up to the developer
to use `@untrackedCaptures` responsibly so that it does not hide visible side effects on mutable state.

Note that the are no restrictions on assignments to local mutable variables, which are not fields of some class. So `@untrackedCaptures` is disallowed for such local variables.

The `untrackedCaptures` annotation can also be used in some other contexts unrelated to mutable variables. These are described in its [doc comment](https://www.scala-lang.org/api/current/scala/caps/unsafe$$untrackedCaptures.html).

## Read-Only Capsets

If we consider subtyping and subcapturing, we observe what looks like a contradiction: `x.rd` is seen as a restricted capability, so `{x.rd}` should subcapture `{x}`. Yet, we have seen in the example above that sometimes it goes the other way: `a`'s capture set is either `{a}` or `{any}`, yet `a` can be used to define `b1` and `b2`, with capture sets `{a.rd}` and `{any.rd}`, respectively.

The contradiction can be explained by noting that we use a capture set in two different roles.

First, and as always, a capture set defines _retained capabilities_ that may or may be not used by a value. More capabilities give larger types, and the empty capture set is the smallest set according to that ordering. That makes sense: If a higher-order function like `map` is willing to accept a function `A => B` that can have arbitrary effects it's certainly OK to pass a pure function of type `A -> B` to it.

But for mutations, we use a capture set in a second role, in which it defines a set of _access permissions_. If we have a `Ref[T]^`, we can access all its methods, but if we have a `Ref[T]^{any.rd}`, we can access only regular methods, not update methods. From that viewpoint a stateful type with exclusive capabilities lets you do more than a stateful type with just read-only capabilities. So by the Liskov substitution principle, sets with exclusive capabilities subcapture sets with only read-only capabilities.

The contradiction can be solved by distinguishing these two roles. For access permissions, we express read-only sets with an additional _qualifier_ `reader`. That qualifier is used only in the formal theory and the implementation, it currently cannot be expressed in source.
We add an implicit read-only qualifier `reader` to all capture sets on stateful types that consist only of shared or read-only capabilities.
So when we write
```scala sc:nocompile
val b1: Ref[A]^{a.rd} = a
```
we really mean
```scala sc:nocompile
val b1: Ref[A]^{a.rd}.reader = a
```

The current implementation shows the implicit `reader` qualifier under the `-Ycc-verbose` setting.

The subcapturing theory for sets is then as before, with the following additional rules:

 - `C <: C.reader`
 - `C₁.reader <: C₂.reader` if `C₍ <: C₂`
 - `{x, ...}.reader = {x.rd, ...}.reader`
 - `{x.rd, ...} <: {x, ...}`

