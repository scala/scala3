---
layout: doc-page
title: "Tracked Capabilities Overview"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/overview.html
---

### Introduction

Tracked capabilities are the most important new feature of Scala. This page gives an overview of
the underlying concepts, focusing on what capabilities can express and what one can do with them.

For a more systematic description of all the details we refer you to the following pages:

 - [Capture Checking Basics](./basics.md): Getting started with capture checking.
 - [Capture Checking of Classes](./classes.md): Rules for capture checking classes and objects.
 - [Capability Polymorphism](./polymorphism.md): Subtyping of capturing types and capture set variables.
 - [Scoping of Capabilities](./scoped-caps.md): Scoping of `any` and `fresh` capabilities.
 - [Capability Classifiers](./classifiers.md): Classifiers for capabilities and projection with the `.only[...]` operator.
 - [Checked Exceptions](./checked-exceptions.md): `CanThrow` capabilities and `throws` clauses.
 - [Stateful Capabilities](./mutability.md): Capabilities for mutable data structures.
 - [Separation Checking](./separation-checking.md): More detailed checking for congtrolling aliasing and sharing of capabilities.
 - [How to Use the Capture Checker](./how-to-use.md): Instructions how to enable and configure capture checking.
 - [Internals](./internals.md): Description of some of the internals of the capture checker for implementers.

Tracked capabilities are supported under Scala's capture checking extension, which can be enabled
by the language import
```scala sc:nocompile
import language.experimental.captureChecking
```
Some more features of capture checking having to do with mutable data structures and alias control currently require a separate language import
```scala sc:nocompile
import language.experimental.separationChecking
```
Both extensions are experimental, which means that details can still change. Capture checking is by now quite mature and we expect it to be stabilized soon. Separation checking is still a bit more fluid at present.

### Capabilities

Informally, a capability is a value "of interest". For instance, a file handle, an access permission token, or a mutable data structure all make sense as capabilities. But the pair `("hello", "world!")` is just a value, not a capability. Often capabilities are associated with effects. For instance, a file handle gives access to the effect of reading or writing it.

One can designate a value as a capability by making the type of the value extend directly or indirectly a standard trait `Capability`. For instance, a `File` can be declared to be a capability like this:
```scala sc:nocompile
	class File(path: String) extends ExclusiveCapability
```
Here, `ExclusiveCapability` is a subtrait of `Capability` that prevents concurrent accesses.

### Capability Tracking

Capabilities in Scala 3 are *tracked*. This means that we record in a type which capabilities can be accessed by values of that type. We write `A^{c}` for the type of values of type `A` that can access the capability `c`.

For instance we can define a class `Logger` for sending log messages to a file and instantiate it like this:
```scala sc:nocompile
  class Logger(f: File) { ... }

  val out = File("~/some/bits")
  val lg: Logger^{out} = Logger(out)
```
Note the type `Logger^{out}` of `lg` above. It indicates not only that `lg` is of class `Logger` but also that it can access file `out`. We also say `lg` _captures_ `out` and call `Logger^{out}` a _capturing type_.

Generally, the type `A^{c₁, ..., cₙ}` stands for instances that retain capabilities `c₁, ..., cₙ`.
If class `A` does not extend `Capability`, then the type `A` alone stands for instances
that retain no capabilities, i.e. `A` is equivalent to `A^{}`. We also say `A` is _pure_.
The opposite of pure `A` describes instances of `A` that can retain arbitrary capabilities.
This type is `A^{any}`, or, shorter, `A^`.

Values of capturing types are themselves considered capabilities. For instance `lg` above is treated as a capability even though its class `Logger` does not extend `Capability`.

Capability sets induce a subtyping relation, where smaller sets lead to smaller types.
Furthermore, if `c` is of a capturing type `A^{c₁, ..., cₙ}` then we also have `{c} <: {c₁, ..., cₙ}`.
We say in this case that `c` _refines_ the underlying capture set `{c₁, ..., cₙ}`.

For instance, if `out` and `lg` are capabilities as defined above and `f` is some other capability, we have
```
  A  <:  A^{lg}  <:  A^{out}  <:  A{out, f}  <:  A^
```


### Function Types

Function types can also be equipped with capability sets.
The function type `A -> B` is considered to be pure, so it cannot
retain any capability. We then use the following shorthands.

```scala sc:nocompile
   A ->{c₁, ..., cₙ} B   =  (A -> B)^{c₁, ..., cₙ}
                A => B   =  A ->{any} B
```

A function captures any capabilities accessed by its body. For instance the function
```scala sc:nocompile
(x: Int) =>
  lg.log(s"called with parameter $x")
  x + 1
```
has type `Int ->{lg} Int`, which is a subtype of `Int => Int`.

Scala systematically distinguishes methods, which are members of classes and objects, from functions, which are objects themselves. Methods don't have expressible types and consequently don't have capability sets that can be tracked. Instead, the capability set is associated with the enclosing object. For instance, in
```scala sc:nocompile
val exec = new Runnable {
  def run() = lg.log(s"called with parameter $x")
}
```
the value `exec` has type `Runnable^{lg}` since `lg` is accessed by `Runnable`'s method `run`.
Methods can be converted to functions by just naming the method without passing any parameters (this is called eta expansion). For instance the value `exec.run` would have type `() ->{lg} Unit`.

### Lifetimes

One consequence of tracking capabilities in types is that we can control their lifetimes.
For instance, here is a function that runs an operation `op` while providing a logger to a fresh
file. After the operation is finished, the file is closed and the result of the operation is returned.
The function is generic: the result type of the operation is the type parameter `T`, which can be instantiated as needed.

```scala sc:nocompile
def logged[T](op: Logger^ => T): T =
  val f = new File("logfile")
  val l = Logger(f)
  val result = op(l)
  f.close()
  result
```

A problematic use of the function would leak the logger `l` in the result of the operation. For instance like this:
```scala sc:nocompile
  val bad = logged { l =>
    () => l.log("too late!"))
  }
  bad()
```
Here, the value of bad is result of the operation passed to `logged` is the nested function `() => l.log("too late!")`. This is also the value of `bad`. Hence, the call `bad()` would invoke `l.log`, but at this point the file underlying the logger was already closed by `logged`. Fortunately, the definition of `bad` is rejected in Scala 3's type system. Essentially, the type parameter `T` in the definition of `logger` must be independent of the identity of the logger passed to `op`. In the bad usage scenario, this requirement is violated since the result type of `op` has type `() ->{l} Unit`, so it does depend on the logger parameter in its capture set.

The fine grained control of lifetimes is one of the properties that set tracked capabilities apart from traditional untracked ones.

### Implicit Capability Passing

Capabilities like `out` or `lg` are objects with which a program interacts as usual. This aspect of object capabilities is one of their strengths since it leads to ergonomic notation. Capabilities are also often used to establish some kind of context that establishes permissions to execute some effects.

For instance, in the [Gears](https://lampepfl.github.io/gears/) framework for concurrent systems we have `Async` capabilities that allow a computation to suspend while waiting for an external event (and possibly be cancelled in the process). This is modeled by having the `Async` class extend a capability trait:

```scala sc:nocompile
   	class Async extends SharedCapability

	// A suspendable method using an Async capability
	def readDataEventually(file: File)(using async: Async): Data
```

One common issue with traditional capabilities is that passing many capabilities as parameters to all the places that need them can get tedious quickly. In Scala this is much less of a problem since capabilities can be passed as implicit parameters via `using` clauses. For instance, the following method calls `readDataEventually` without having to pass the parameter `async` explicitly.
```scala sc:nocompile
def processData(using Async) =
  val file = File("~/some/path")
  readDataEventually(file)
```
Since the parameter is not mentioned, we also don’t need a name for it in its definition. So the method above is a convenient shorthand for the following more explicit definition.

```scala sc:nocompile
def processData(using async: Async) =
  val file = File("~/some/path")
  readDataEventually(file)(using async)
```

### Mutation

Mutable variables and mutable data structures are also considered capabilities.
For instance, consider a pair of functions for incrementing and reading a counter:
```scala sc:nocompile
var counter: Int = 0
def incr = () => counter += 1
def current = () => counter
```
Function `incr` has type `() ->{counter} Unit`, which records the fact that the counter is updated when calling the function.

We distinguish read and write accesses to mutable data. A read access to a mutable data stricture `m` charges a "fractional" read-only capability `m.rd` whereas a write access charges the full capability `m`. [Separation checking](./separation-checking.md) ensures that targets to write accesses cannot be obscured through aliasing. This is analogous to borrow checking in Rust, but uses a different mechanism based on capabilities instead of regions.

Mutable data structures extend trait `Mutable`, which is another subtrait of `Capability`.
Methods that write to such data are marked with an `update` modifier. For instance, here is
a class for append-buffers:

```scala sc:nocompile
class Buffer[T] extends Mutable {
  update def append(elem: T): Unit
  def apply(pos: Int): T
  def size: Int
}
```
The `append` method comes with an `update` modifier since it changes the contents of the `Buffer`.
By contrast, `apply` and `size` are regular methods that are guaranteed update-free.

Types are used to regulate calls to update methods. A reference of type `Buffer` only allows access to regular methods. If the reference has type `Buffer^` it also allows access to update methods.

For instance, a `copy` method between buffers could be written like this:

```scala sc:nocompile
def copy(from: Buffer[T], to: Buffer[T]^): Unit =
  for i <- 0 until from.size do
    to.append(from(i))
```

The type of the `Buffer` arguments makes it clear which buffer is modified and which buffer is read-only. Note that the distinction between `C` and `C^` applies to all types `C` extending trait `Mutable`. For other capability types the two forms are equivalent.

### Global Capabilities

In traditional object capability systems, global capabilities are ruled out. Indeed, if we rely on controlling accesses simply by scoping rules, global capabilities don't make much sense since they allow unrestricted access everywhere.

But with tracked capabilities, we have another means to control access via tracked types. Consequently global capabilities can be allowed. For instance,
here is a `Console` object:
```scala sc:nocompile
object Console {
  val in: File = ...
  val out: File = ...
}
```
Here, `in`, and `out` are of type `File`, so `Console.in`, and `Console.out` are global capabilities. A function `() => Console.out.println("hi")` would have type
`() ->{Console.out} Unit`. It could not be passed into a context expecting a pure function.

Allowing global capabilities like `Console.out` is quite useful since it means that we don't need to fundamentally change a system's architecture to make it capability-safe. In traditional capability systems all capabilities provided by the host system have to be passed as parameters into the main entry point and from there to all functions that need access. This usually requires a global refactoring of the code base and can lead to more complex code.

### Summary

Scala 3's capabilities are both more convenient and more expressive than traditional ones. They are more convenient since `using` clauses allow to pass capabilities implicitly down a call chain and global capabilities reduce the need for such call chains in the first place. And they are more expressive, since tracked capabilities allow to describe and constrain the effects of computations. Furthermore, one can distinguish read-only and update capabilities and one can ensure that updates are only to unshared references.
