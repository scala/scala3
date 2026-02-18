---
layout: doc-page
title: "Capture Checking Basics"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/basics.html
---

## Introduction

Capture checking can be enabled by the language import
```scala sc:nocompile
import language.experimental.captureChecking
```
At present, capture checking is still highly experimental and unstable, and it evolves quickly.
Before trying it out, make sure you have the latest version of Scala.

To get an idea what capture checking can do, let's start with a small example:
```scala
//{
import java.io.FileOutputStream
//}
def usingLogFile[T](op: FileOutputStream => T): T =
  val logFile = FileOutputStream("log")
  val result = op(logFile)
  logFile.close()
  result
```
The `usingLogFile` method invokes a given operation with a fresh log file as parameter. Once the operation has ended, the log file is closed and the
operation's result is returned. This is a typical _try-with-resources_ pattern, similar to many other such patterns which are often supported by special language constructs in other languages.

The problem is that `usingLogFile`'s implementation is not entirely safe. One can
undermine it by passing an operation that performs the logging at some later point
after it has terminated. For instance:
```scala sc:nocompile
val later = usingLogFile { file => () => file.write(0) }
later() // crash
```
When `later` is executed it tries to write to a file that is already closed, which
results in an uncaught `IOException`.

Capture checking gives us the mechanism to prevent such errors _statically_. To
prevent unsafe usages of `usingLogFile`, we can declare it like this:
```scala sc:nocompile
def usingLogFile[T](op: FileOutputStream^ => T): T =
  // same body as before
```
The only thing that's changed is that the `FileOutputStream` parameter of `op` is now
followed by `^`. We'll see that this turns the parameter into a _capability_ whose lifetime is tracked.

If we now try to define the problematic value `later`, we get a static error:
```
   |  val later = usingLogFile { f => () => f.write(0) }
   |                           ^^^^^^^^^^^^^^^^^^^^^^^^^
   |  Found:    (f: java.io.FileOutputStream^'s1) ->'s2 () ->{f} Unit
   |  Required: java.io.FileOutputStream^ => () ->'s3 Unit
   |
   |  Note that capability f cannot be included in outer capture set 's3.
```
In this case, it was easy to see that the `logFile` capability escapes in the closure passed to `usingLogFile`. But capture checking also works for more complex cases.
For instance, capture checking is able to distinguish between the following safe code:
```scala sc:nocompile
val xs = usingLogFile { f =>
  List(1, 2, 3).map { x => f.write(x); x * x }
}
```
and the following unsafe one:
```scala sc:nocompile
val xs = usingLogFile { f =>
  LzyList(1, 2, 3).map { x => f.write(x); x * x }
}
```
An error would be issued in the second case, but not the first one (this assumes a capture-aware
formulation `LzyList` of lazily evaluated lists, which we will present later in the chapter
on [capture checking classes](classes.md)).

It turns out that capture checking has very broad applications. Besides the various
try-with-resources patterns, it can also be a key part to the solutions of many other long standing problems in programming languages. Among them:

 - How to have a simple and flexible system for checked exceptions. We show later
   how capture checking enables a clean and fully safe system for checked exceptions in Scala.
 - How to address the problem of effect polymorphism in general.
 - How to solve the "what color is your function?" problem of mixing synchronous
   and asynchronous computations.
 - How to do region-based allocation, safely.
 - How to reason about capabilities associated with memory locations.

The following sections explain in detail how capture checking works in Scala 3.


## Capabilities and Capturing Types

The capture checker extension introduces a new kind of types and it enforces some rules for working with these types.


Capture checking is done in terms of _capturing types_ of the form
`T^{c₁, ..., cᵢ}`. Here `T` is a type, and `{c₁, ..., cᵢ}` is a _capture set_ consisting of references to capabilities `c₁, ..., cᵢ`.

An _object capability_ is syntactically a method- or class-parameter, a local variable, or the `this` of an enclosing class. The type of a capability
must be a capturing type with a non-empty capture set. We also say that
variables that are capabilities are _tracked_.

In a sense, every
capability gets its authority from some other, more sweeping capability which it captures. The recursion stops with a _universal capability_,  written `any`, from which all other capabilities are ultimately derived.
If `T` is a type, then `T^` is a shorthand for `T^{any}`, meaning `T` can capture arbitrary capabilities.

Here is an example:
```scala sc:nocompile
class FileSystem

class Logger(fs: FileSystem^):
  def log(s: String): Unit = ... // Write to a log file, using `fs`

def test(fs: FileSystem^) =
  val l: Logger^{fs} = Logger(fs)
  l.log("hello world!")
  val xs: LzyList[Int]^{l} =
    LzyList.from(1)
      .map { i =>
        l.log(s"computing elem # $i")
        i * i
      }
  xs
```
Here, the `test` method takes a `FileSystem` as a parameter. `fs` is a capability since its type has a non-empty capture set. The capability is passed to the `Logger` constructor
and retained as a field in class `Logger`. Hence, the local variable `l` has type
`Logger^{fs}`: it is a `Logger` which retains the `fs` capability.

The second variable defined in `test` is `xs`, a lazy list that is obtained from
`LzyList.from(1)` by logging and mapping consecutive numbers. Since the list is lazy,
it needs to retain the reference to the logger `l` for its computations. Hence, the
type of the list is `LzyList[Int]^{l}`. On the other hand, since `xs` only logs but does
not do other file operations, it retains the `fs` capability only indirectly. That's why
`fs` does not show up in the capture set of `xs`.

Capturing types come with a subtype relation where types with "smaller" capture sets are subtypes of types with larger sets (the _subcapturing_ relation is defined in more detail below). If a type `T` does not have a capture set, it is called _pure_, and is a subtype of
any capturing type that adds a capture set to `T`.

## Function Types

The usual function type `A => B` now stands for a function that can capture arbitrary capabilities. We call such functions
_impure_. By contrast, the new single arrow function type `A -> B` stands for a function that cannot capture any capabilities, or otherwise said, is _pure_.
One can add a capture set after the arrow of an otherwise pure function.
For instance, `A ->{c, d} B` would be a function that can capture capabilities `c` and `d`, but no others.
This type is a shorthand for `(A -> B)^{c, d}`, i.e. the function type `A -> B` with possible captures `{c, d}`.

The impure function type `A => B` is treated as an alias for `A ->{any} B`. That is, impure functions are functions that can capture anything.

A capture annotation `^` binds more strongly than a function arrow. So
`A -> B^{c}` is read as `A -> (B^{c})` and `A -> B^` is read as `A -> (B^)`.

Analogous conventions apply to context function types. `A ?=> B` is an impure context function, with `A ?-> B` as its pure complement.

**Note 1:** The identifiers `->` and `?->` are now treated as soft keywords when used as infix type operators. They are
still available as regular identifiers for terms. For instance, the mapping syntax `Map("x" -> 1, "y" -> 2)` is still supported since it only applies to terms.

**Note 2:** The distinctions between pure vs impure function types do not apply to methods. In fact, since methods are not values they never capture anything directly. References to
capabilities in a method are instead counted in the capture set of the enclosing object.

## By-Name Parameter Types

A convention analogous to function types also extends to by-name parameters. In
```scala sc:nocompile
def f(x: => Int): Int
```
the actual argument can refer to arbitrary capabilities. So the following would be OK:
```scala sc:nocompile
f(if p(y) then throw Ex() else 1)
```
On the other hand, if `f` was defined like this
```scala sc:nocompile
def f(x: -> Int): Int
```
the actual argument to `f` could not refer to any capabilities, so the call above would be rejected.
One can also allow specific capabilities like this:
```scala sc:nocompile
def f(x: ->{c} Int): Int
```
Here, the actual argument to `f` is allowed to use the `c` capability but no others.

## Lazy Vals

Lazy vals receive special treatment under capture checking, similar to parameterless methods. A lazy val has two distinct capture sets:

1. **The initializer's capture set**: What capabilities the initialization code uses
2. **The result's capture set**: What capabilities the lazy val's value captures

### Initializer Captures

When a lazy val is declared, its initializer is checked in its own environment (like a method body). The initializer can capture capabilities, and these are tracked separately:

```scala sc:nocompile
def example(console: Console^) =
  lazy val x: () -> String =
    console.println("Computing x")  // console captured by initializer
    () => "Hello, World!"           // result doesn't capture console

  val fun: () ->{console} String = () => x()   // ok: accessing x uses console
  val fun2: () -> String = () => x()           // error: x captures console
```

Here, the initializer of `x` uses `console` (to print a message), so accessing `x` for the first time will use the `console` capability. However, the **result** of `x` is a pure function `() -> String` that doesn't capture any capabilities.

The type system tracks that accessing `x` requires the `console` capability, even though the resulting value doesn't. This is reflected in the function types: `fun` must declare `{console}` in its capture set because it accesses `x`.

### Lazy Val Member Selection

When accessing a lazy val member through a qualifier, the qualifier is charged to the current capture set, just like calling a parameterless method:

```scala sc:nocompile
trait Container:
  lazy val lazyMember: String

def client(c: Container^): Unit =
  val f1: () -> String = () => c.lazyMember        // error
  val f2: () ->{c} String = () => c.lazyMember     // ok
```

Accessing `c.lazyMember` can trigger initialization, which may use capabilities from `c`. Therefore, the capture set must include `c`.

### Equivalence with Methods

For capture checking purposes, lazy vals behave identically to parameterless methods:

```scala sc:nocompile
trait T:
  def methodMember: String
  lazy val lazyMember: String

def test(t: T^): Unit =
  // Both require {t} in the capture set
  val m: () ->{t} String = () => t.methodMember
  val l: () ->{t} String = () => t.lazyMember
```

This equivalence reflects that both can trigger computation using capabilities from their enclosing object.

## Subtyping and Subcapturing

Capturing influences subtyping. As usual we write `T₁ <: T₂` to express that the type
`T₁` is a subtype of the type `T₂`, or equivalently, that `T₁` conforms to `T₂`. An
analogous _subcapturing_ relation applies to capture sets. If `C₁` and `C₂` are capture sets, we write `C₁ <: C₂` to express that `C₁` _is covered by_ `C₂`, or, swapping the operands, that `C₂` _covers_ `C₁`.

Subtyping extends as follows to capturing types:

 - Pure types are subtypes of capturing types. That is, `T <: T ^ C`, for any type `T`, capturing set `C`.
 - For capturing types, smaller capturing sets produce subtypes: `T₁ ^ C₁ <: T₂ ^ C₂` if
   `C₁ <: C₂` and `T₁ <: T₂`.

A subcapturing relation `C₁ <: C₂` holds if `C₂` _accounts for_ every element `c` in `C₁`. This means one of the following three conditions must be true:

 - `c ∈ C₂`,
 - `c` refers to a parameter of some class `Cls` and `C₂` contains `Cls.this`,
 - `c`'s type has capturing set `C` and `C₂` accounts for every element of `C` (that is, `C <: C₂`).


**Example 1.** Given
```scala sc:nocompile
fs: FileSystem^
ct: CanThrow[Exception]^
l : Logger^{fs}
```
we have
```
{l}  <: {fs}     <: {any}
{fs} <: {fs, ct} <: {any}
{ct} <: {fs, ct} <: {any}
```
The set consisting of the root capability `{any}` covers every other capture set. This is
a consequence of the fact that, ultimately, every capability is created from `any`.

**Example 2.** Consider again the FileSystem/Logger example from before. `LzyList[Int]` is a proper subtype of `LzyList[Int]^{l}`. So if the `test` method in that example
was declared with a result type `LzyList[Int]`, we'd get a type error. Here is the error message:
```
11 |def test(using fs: FileSystem^): LzyList[Int] = {
   |                                                 ^
   |                                        Found:    LzyList[Int]^{fs}
   |                                        Required: LzyList[Int]
```
Why does it say `LzyList[Int]^{fs}` and not `LzyList[Int]^{l}`, which is, after all, the type of the returned value `xs`? The reason is that `l` is a local variable in the body of `test`, so it cannot be referred to in a type outside that body. What happens instead is that the type is _widened_ to the smallest supertype that does not mention `l`. Since `l` has capture set `fs`, we have that `{fs}` covers `{l}`, and `{fs}` is acceptable in a result type of `test`, so `{fs}` is the result of that widening.
This widening is called _avoidance_; it is not specific to capture checking but applies to all variable references in Scala types.

## Capability Classes

Classes like `CanThrow` or `FileSystem` have the property that their values are always intended to be capabilities. We can make this intention explicit and save boilerplate by letting these classes extend the  `SharedCapability` class defined in object `caps`.

A type extending `SharedCapability` always comes with a capture set. If no capture set is given explicitly, we assume the capture set is `{any}`.

This means we could equivalently express the `FileSystem` and `Logger` classes as follows:

```scala sc:nocompile
import caps.SharedCapability

class FileSystem extends SharedCapability

class Logger(using FileSystem):
  def log(s: String): Unit = ???

def test(using fs: FileSystem) =
  val l: Logger^{fs} = Logger()
  ...
```
In this version, `FileSystem` is a capability class, which means that the occurrences of `FileSystem` in the types of the parameters of `Logger` and `test` are implicitly expanded to `FileSystem^`. On the other hand, types like `FileSystem^{f}` or
`FileSystem^{}` are kept as written.

Another, unrelated change in the last version of the `Logger` example is that the `FileSystem` capability is now passed as an implicit parameter. It is quite natural to model capabilities with implicit parameters since it greatly reduces the wiring overhead once multiple capabilities are in play.

## Escape Checking

Capabilities follow the usual scoping discipline, which means that capture sets
can contain only capabilities that are visible at the point where the set is defined.

We now reconstruct how this principle produced the error in the introductory example, where
`usingLogFile` was declared like this:
```scala sc:nocompile
def usingLogFile[T](op: FileOutputStream^ => T): T = ...
```
The error message was:
```
   |  val later = usingLogFile { f => () => f.write(0) }
   |                           ^^^^^^^^^^^^^^^^^^^^^^^^^
   |  Found:    (f: java.io.FileOutputStream^'s1) ->'s2 () ->{f} Unit
   |  Required: java.io.FileOutputStream^ => () ->'s3 Unit
   |
   |  Note that capability f cannot be included in outer capture set 's3.
```
This error message was produced by the following logic:

 - The `f` parameter has type `FileOutputStream^`, which makes it a capability.
 - Therefore, the type of the expression `() => f.write(0)` is `() ->{f} Unit`.
 - This makes the type of the whole closure passed to `usingLogFile` the dependent function type
   `(f: FileOutputStream^'s1) ->'s2 () ->{f} Unit`,
   for some as yet uncomputed capture sets `'s1` and `'s2`.
 - The expected type of the closure is a simple, parametric, impure function type `FileOutputStream^ => T`,
   for some instantiation of the type variable `T`.
 - Matching with the found type, `T` must have the shape `() ->'s3 Unit`, for
   some capture set `'s3` defined at the level of value `later`.
 - That capture set cannot include the capability `f` since `f` is locally bound.
   This causes the error.

An analogous restriction applies to the type of a mutable variable.
Another way one could try to undermine capture checking would be to
assign a closure with a local capability to a global variable. Maybe
like this:
```scala sc:nocompile
var loophole: () => Unit = () => ()
usingLogFile { f =>
  loophole = () => f.write(0)
}
loophole()
```
But this will not compile either, since the capture set of the mutable variable `loophole` cannot refer to variable `f`, which is not visible
where `loophole` is defined.

### Monotonicity Rule

Looking at object graphs, we observe a monotonicity property: The capture set of an object `x` covers the capture sets of all objects reachable through `x`. This property is reflected in the type system by the following _monotonicity rule_:

 - In a class `C` with a field `f`, the capture set `{this}` covers the capture set `{this.f}` as well as the capture set of any application of `this.f` to pure arguments.
