---
layout: doc-page
title: "Capability Polymorphism"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/polymorphism.html
---

## Introduction

Capture checking supports capture-polymorphic programming in two complementary styles:

1.	**Implicit** capture polymorphism, which is the default and has minimal syntactic overhead.
1.	**Explicit** capture polymorphism, which allows programmers to abstract over capture sets directly through explicit generic parameters.

The difference between implicit and explicit capture polymorphism is analogous to the difference
between polymorphism through subtyping versus parametric polymorphism through type parameters/generics.

### Implicit Polymorphism

In many cases, such a higher-order functions, we do not need new syntax to be polymorphic over
capturing types. The classic example is `map` over lists:
```scala
trait List[+A]:
  // Works for pure functions AND capturing functions!
  def map[B](f: A => B): List[B]
```
Due to the conventions established in previous sections, `f: A => B` translates to `f: A ->{cap} B`
under capture checking which means that the function argument `f` can capture any capability, i.e.,
`map` will have `f`'s effects, if we think of capabilities as the only means to induce side effects,
then _capability polymorphism equals effect polymorphism_. By careful choice of notation and the
[capture tunneling](classes.md#capture-tunneling) mechanism for generic types, we get effect
polymorphism _for free_, and no signature changes are necessary on an eager collection type
such as `List`.

Contrasting this against lazy collections such as `LzyList` from the [previous section](classes.md),
the implicit capability polymorphism induces an additional capture set on the result of `map`:
```scala
extension [A](xs: LzyList[A]^)
  def map[B](f: A => B): LzyList[B]^{xs, f}
```
Unlike the eager version which only uses `f` during the computation, the lazy counterpart delays the
computation, so that the original list and the function are captured by the result.
This relationship can be succinctly expressed due to the path-dependent result capture set
`{xs, f}` and would be rather cumbersome to express in more traditional effect-type systems
with explicit generic effect parameters.

### Explicit Polymorphism

In some situations, it is convenient or necessary to parameterize definitions by a capture set.
This allows an API to state precisely which capabilities its clients may use. Consider a `Source`
that stores `Listeners`:
```scala
class Source[X^]:
  private var listeners: Set[Listener^{X}] = Set.empty
  def register(x: Listener^{X}): Unit =
    listeners += x

  def allListeners: Set[Listener^{X}] = listeners
```
Here, `X^` is a _capture-set variable_. It may appear inside capture sets throughout the class body.
The field listeners holds exactly the listeners that capture X, and register only accepts such
listeners.

#### Under the hood

Capture-set variables without user-provided bounds range over the interval
 `>: {} <: {caps.cap}` which is the full lattice of capture sets. They behave like type parameters
 whose domain is "all capture sets", not all types.

Under the hood, a capture-set variable is implemented as a normal type parameter with special bounds:
```scala
class Source[X >: CapSet <: CapSet^]:
  ...
```
`CapSet` is a sealed marker trait in `caps` used internally to distinguish capture-set variables. It
cannot be instantiated or extended; in non-capture-checked code, `CapSet^{a}` and `CapSet^{a,b}`
erase to plain `CapSet`, while with capture checking enabled their capture sets remain distinct.
This representation is an implementation detail and should not be used directly, as `CapSet` might
be erased entirely by the compiler in the future.

#### Instantiation and inference
Capture-set variables are inferred in the same way as ordinary type variables.
They can also be instantiated explicitly with capture-set literals or other
capture-set variables:
```scala
class Async extends caps.SharedCapability

def listener(a: Async): Listener^{a} = ???

def test1[X^](async1: Async, others: List[Async^{X}]) =
  val src = Source[{async1, X}]
  src.register(listener(async1))
  others.map(listener).foreach(src.register)
  val ls: Set[Listener^{async1, X}] = src.allListeners
```
Here, `src` accepts listeners that may capture either the specific capability `async1` or any element of
others. The resulting `allListeners` method reflects this relationship.

#### Transforming collections
A typical use of explicit capture parameters arises when transforming collections of capturing
values, such as `Future`s. In these cases, the API must guarantee that whatever capabilities are
captured by the elements of the input collection are also captured by the elements of the output.

The following example takes an unordered `Set` of futures and produces a `Stream` that yields their
results in the order in which the futures complete. Using an explicit capture variable `C^`, the
signature expresses that the cumulative capture set of the input futures is preserved in the
resulting stream:
```scala
def collect[T, C^](fs: Set[Future[T]^{C}])(using Async^): Stream[Future[T]^{C}] =
  val channel = Channel()
  fs.forEach.(_.onComplete(v => channel.send(v)))
  Stream.of(channel)
```

#### Tracking the evolution of mutable objects
A common use case for explicit capture parameters is when a mutable object’s reachable capabilities
_grow_ due to mutation. For example, concatenating effectful iterators:
```scala
class ConcatIterator[A, C^](var iterators: mutable.List[IterableOnce[A]^{C}]):
  def concat(it: IterableOnce[A]^): ConcatIterator[A, {C, it}]^{this, it} =
    iterators ++= it                             //            ^
    this                                         // track contents of `it` in the result
```
In such cases, the type system must ensure that any existing aliases of the iterator become invalid
after mutation. This is handled by [mutation tracking](mutability.md) and [separation tracking](separation-checking.md), which are currently under development.

## Shall I Be Implicit or Explicit?

Implicit capability polymorphism is intended to cover the most common use cases.
It integrates smoothly with existing functional programming idioms and was expressive enough to
retrofit the Scala standard collections library to capture checking with minimal changes.

Explicit capability polymorphism is introduced only when the capture relationships of an API must be
stated directly in its signature. At this point, we have seen several examples where doing so improves
clarity: naming a capture set explicitly, preserving the captures of a collection, or describing how
mutation changes the captures of an object.

The drawback of explicit polymorphism is additional syntactic overhead. Capture parameters can make
signatures more verbose, especially in APIs that combine several related capture sets.

**Recommendation:** Prefer implicit polymorphism by default.
Introduce explicit capture parameters only when the intended capture relationships cannot be expressed
implicitly or would otherwise be unclear.

## Capability Members

Capture parameters can also be introduced as *capability members*, in the same way that type
parameters can be replaced with type members. The earlier example
```scala
class Source[X^]:
  private var listeners: Set[Listener^{X}] = Set.empty
```
can be written instead as:
```scala
class Source:
  type X^
  private var listeners: Set[Listener^{this.X}] = Set.empty

  def register(l: Listener^{this.X]): Unit =
    listeners += l

  def allListeners: Set[Listener^{this.X}] = listeners
```
A capability member behaves like a path-dependent capture-set variable. It may appear in capture
annotations using paths such as `{this.X}`.

Capability members can also have capture-set bounds, restricting which capabilities they may contain:
```scala
trait Reactor:
  type Cap^ <: {caps.cap}
  def onEvent(h: Event ->{this.Cap} Unit): Unit
```
Each implementation of Reactor may refine `Cap^` to a more specific capture set:
```scala
trait GUIReactor extends Reactor:
  type Cap^ <: {ui, log}
```
Here, `GUIReactor` specifies that event handlers may capture only `ui`, `log`, or a subset thereof.
The `onEvent` method expresses this via the path-dependent capture set `{this.Cap}`.

Capability members are useful when capture information should be tied to object identity or form part
of an abstract interface, instead of being expressed through explicit capture parameters.

**Advanced uses:** We discuss more advanced use cases for capability members [here](advanced.md).
