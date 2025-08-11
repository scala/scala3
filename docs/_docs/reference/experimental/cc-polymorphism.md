---
layout: doc-page
title: "Capability Polymorphism"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/cc-polymorphism.html
---

## Capability Polymorphism

It is sometimes convenient to write operations that are parameterized with a capture set of capabilities. For instance consider a type of event sources
`Source` on which `Listener`s can be registered. Listeners can hold certain capabilities, which show up as a parameter to `Source`:
```scala
class Source[X^]:
  private var listeners: Set[Listener^{X}] = Set.empty
  def register(x: Listener^{X}): Unit =
    listeners += x

  def allListeners: Set[Listener^{X}] = listeners
```
The type variable `X^` can be instantiated with a set of capabilities. It can occur in capture sets in its scope. For instance, in the example above
we see a variable `listeners` that has as type a `Set` of `Listeners` capturing `X`. The `register` method takes a listener of this type
and assigns it to the variable.

Capture-set variables `X^` without user-annotated bounds by default range over the interval `>: {} <: {caps.cap}` which is the universe of capture sets instead of regular types.

Under the hood, such capture-set variables are represented as regular type variables within the special interval
 `>: CapSet <: CapSet^`.
For instance, `Source` from above could be equivalently
defined as follows:
```scala
class Source[X >: CapSet <: CapSet^]:
  ...
```
`CapSet` is a sealed trait in the `caps` object. It cannot be instantiated or inherited, so its only
purpose is to identify type variables which are capture sets. In non-capture-checked
usage contexts, the type system will treat `CapSet^{a}` and `CapSet^{a,b}` as the type `CapSet`, whereas
with capture checking enabled, it will take the annotated capture sets into account,
so that `CapSet^{a}` and `CapSet^{a,b}` are distinct.
This representation based on `CapSet` is subject to change and
its direct use is discouraged.

Capture-set variables can be inferred like regular type variables. When they should be instantiated
explicitly one supplies a concrete capture set. For instance:
```scala
class Async extends caps.SharedCapability

def listener(async: Async): Listener^{async} = ???

def test1(async1: Async, others: List[Async]) =
  val src = Source[{async1, others*}]
  ...
```
Here, `src` is created as a `Source` on which listeners can be registered that refer to the `async` capability or to any of the capabilities in list `others`. So we can continue the example code above as follows:
```scala
  src.register(listener(async1))
  others.map(listener).foreach(src.register)
  val ls: Set[Listener^{async, others*}] = src.allListeners
```
A common use-case for explicit capture parameters is describing changes to the captures of mutable fields, such as concatenating
effectful iterators:
```scala
class ConcatIterator[A, C^](var iterators: mutable.List[IterableOnce[A]^{C}]):
  def concat(it: IterableOnce[A]^): ConcatIterator[A, {C, it}]^{this, it} =
    iterators ++= it                             //            ^
    this                                         // track contents of `it` in the result
```
In such a scenario, we also should ensure that any pre-existing alias of a `ConcatIterator` object should become
inaccessible after invoking its `concat` method. This is achieved with mutation and separation tracking which are
currently in development.

## Capability Members

Just as parametrization by types can be equally expressed with type members, we could
also define the `Source[X^]` class above could using a _capability member_:
```scala
class Source:
  type X^
  private var listeners: Set[Listener^{this.X}] = Set.empty
  ... // as before
```
Here, we can refer to capability members using paths in capture sets (such as `{this.X}`). Similarly to type members,
capability members can be upper- and lower-bounded with capture sets:
```scala
trait Thread:
  type Cap^
  def run(block: () ->{this.Cap} -> Unit): Unit

trait GPUThread extends Thread:
  type Cap^ >: {cudaMalloc, cudaFree} <: {caps.cap}
```
Since `caps.cap` is the top element for subcapturing, we could have also left out the
upper bound: `type Cap^ >: {cudaMalloc, cudaFree}`.

----

[More Advanced Use Cases](cc-advanced.md)
