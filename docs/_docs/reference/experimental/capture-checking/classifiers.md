---
layout: doc-page
title: "Capability Classifiers"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/classifiers.html
---

## Introduction

Capabilities are extremely versatile. They can express concepts from many different domains. Exceptions, continuations, I/O, mutation, information flow, security permissions, are just some examples, the list goes on.

Sometimes it is important to restrict, or: _classify_ what kind of capabilities are expected or returned in a context. For instance, we might want to allow only control capabilities such as `CanThrow`s or boundary `Label`s but no
other capabilities. Or might want to allow mutation, but no other side effects. This is achieved by having a capability class extend a _classifier_.

For instance, the `scala.caps` package defines a classifier trait called `Control`,
like this:
```scala sc:nocompile
  trait Control extends SharedCapability, Classifier
```
The [Gears library](https://lampepfl.github.io/gears/) then defines a capability class `Async` which extends `Control`.

```scala sc:nocompile
  trait Async extends Control
```
Unlike normal inheritance, classifiers also restrict the capture set of a capability. For instance, say we have a function
```scala sc:nocompile
  def f(using async: Async^) = body
```
(the `^` is as usual redundant here since `Async` is a capability trait).
Then we have the guarantee that any actual `async` argument can only capture
capabilities that have types extending `Control`. No other capabilities such as mutation or I/O are allowed.

A class or trait becomes a classifier by extending directly the marker trait
`caps.Classifier`. So with the definitions above, `Control` is a classifier trait, but `Async` is not, since it extends `Classifier` only indirectly, through `Control`.

Classifiers are unique: a class cannot extend directly or transitively at the same time two unrelated classifier traits. So if a class transitively extends two classifier traits `C1` and `C2` then one of them must be a subtrait of the other.

### Predefined Classifiers

The `caps` object defines the `Classifier` trait itself and some traits that extend it:
```scala sc:nocompile
trait Classifier

sealed trait Capability

trait SharedCapability extends Capability Classifier
trait Control extends SharedCapability, Classifier

trait ExclusiveCapability extends Capability
trait Stateful extends ExclusiveCapability
trait Unscoped extends Stateful, Classifier
```
Here is a graph showing the hierarchy of predefined capability traits. Classifier traits are underlined.
```
              Capability
              /        \
             /          \
            /            \
           /              \
 SharedCapability     ExclusiveCapability
 ----------------            |
        |                    |
        |                 Stateful
        |                    |
        |                    |
     Control              Unscoped
     -------              --------
```
At the top of the hierarchy, we distinguish between _shared_ and _exclusive_ capabilities in two traits `SharedCapability` and `ExclusiveCapability`. All capability classes we have seen so far are shared.
`ExclusiveCapability` is a base trait for capabilities that
are checked for anti-aliasing restrictions with the rules governed by [separation checking](separation-checking.md). Separation checking is currently an optional extension of capture checking, enabled by a different language import. Since `Capability` is a sealed trait, all capability classes are either shared or exclusive. `SharedCapability` is a classifier, but `ExclusiveCapability` is not. Therefore,
exclusive capabilities can have shared capabilities in their capture set but not _vice versa_.

`Control` capabilities are shared. This means they cannot directly or indirectly capture exclusive capabilities such as capabilities that control access to mutable state. Typical `Control` capabilities are:

 - `Label`s that enable to return from a `boundary`,
 - `CanThrow` capabilities that enable throwing exceptions, or
 - `Async` capabilities that allow to suspend.

These are all expressed by having their capability classes extend `Control`.


### Classifier Restriction

Consider the following problem: The `Try.apply` method takes in its `body` parameter a computation, and runs it while catching any exceptions or `boundary.break` aborts. The exception or break will be re-issued when calling
the `get` method of a `Try` object. What should a capability-aware signature of `Try` be?

The body passed to `Try.apply` can have arbitrary effects, so it can retain arbitrary capabilities. Yet the resulting `Try` object will retain only those capabilities of `body` which are classified as `Control`. So the signature of `Try.apply` should look like this:
```scala sc:nocompile
object Try:
  def apply[T](body: => T): Try[T]^{body.only[Control]}
```
Note a new form of capability in the result's capture set: `body.only[Control]`. This is called a _restricted capability_. The general form of a restricted capability is
`c.only[A]` where

 - `c` is a regular, unrestricted capability
 - `A` is a classifier trait.

 When substituting a capability set for the underlying capability, we drop all capabilities that are known to be unrelated to the classifier. For instance, say we have an actual body `{ expr }` that uses a capture set `{io, async}` where

 - `io` is an `IO` capability, where IO is assumed to extend `SharedCapability` but not `Control`.
 - `async` is a `Control` capability.

Then the result of `Try { expr }` would have type `Try^{async}`. We drop `io` since
`io`'s type is a `Capability` class that does not extend `Control`.

If `expr` would use an additional capability `proc: () => Unit`, then `proc` would also show up in the result capture set. Since `proc` is fully effect-polymorphic, we can't exclude that it retains `Control` capabilities, so we have to keep it in the restricted capture set. These elements are shown together in the following example:
```scala sc:nocompile
class IO extends caps.SharedCapability
class Async extends caps.Control

def test(io: IO, async: Async, proc: () => Unit) =
  val r = Try:
    // code accessing `io`, `async`, and `proc` and returning an `Int.
  val _: Try[Int]^{async, proc} = r
```
