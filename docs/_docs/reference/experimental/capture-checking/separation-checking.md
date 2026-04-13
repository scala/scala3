---
layout: doc-page
title: "Separation Checking"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/separation-checking.html
---

## Introduction

Separation checking is an extension of capture checking that enforces unique, un-aliased access to capabilities. It is enabled by a language import
```scala sc:nocompile
import language.experimental.separationChecking
```
(or the corresponding setting `-language:experimental.separationChecking`).
The import has to be given in addition to the `language.experimental.captureChecking` import that enables capture checking.
The reason for the second language import is that separation checking is less mature than capture checking proper, so we are less sure
we got the balance of safety and expressivity right for it at the present time.

The purpose of separation checking is to ensure that certain accesses to capabilities are not aliased. As an example, consider matrix multiplication. A method that multiplies matrices `a`, and `b` into `c`,  could be declared like this:
```scala sc:nocompile
def multiply(a: Matrix, b: Matrix, c: Matrix): Unit
```
But that signature alone does not tell us which matrices are supposed to be inputs and which one is the output. Nor does it guarantee that an input matrix is not re-used as the output, which would lead to wrong results.

With separation checking, we can declare `Matrix` as a `Mutable` type, like this:
```scala sc:nocompile
class Matrix(nrows: Int, ncols: Int) extends Mutable:
  update def setElem(i: Int, j: Int, x: Double): Unit = ...
  def getElem(i: Int, j: Int): Double = ...
```
We further declare that the `setElem` method in a `Matrix` is an `update` method, which means it has a side effect.

Separation checking gives a special interpretation to the following modified signature of `multiply`:
```scala sc:nocompile
def multiply(a: Matrix, b: Matrix, c: Matrix^): Unit
```
In fact, only a single character was added: `c`'s type now carries a universal capability. This signature enforces at the same time two desirable properties:

 - Matrices `a`, and `b` are read-only; `multiply` will not call their update method. By contrast, the `c` matrix can be updated.
 - Matrices `a` and `b` are different from matrix `c`, but `a` and `b` could refer to the same matrix.

So, effectively, anything that can be updated must be unaliased.

## Separation Checking

The idea behind separation checking is simple: We now interpret each occurrence of `any` as a separate top capability. This includes derived syntaxes like `A^` and `B => C`. We further keep track during capture checking which capabilities are subsumed by each `any`. If capture checking widens a capability `x` to a top capability `anyᵢ`, we say `x` is _hidden_ by `anyᵢ`. The rule then is that any capability hidden by a top capability `anyᵢ` cannot be referenced independently or hidden in another `anyⱼ` in code that can see `anyᵢ`.

Here's an example:
```scala sc:nocompile
val x: C^ = y
  ... x ...  // ok
  ... y ...  // error
```
This principle ensures that capabilities such as `x` that have `any` as underlying capture set are un-aliased or "fresh". Any previously existing aliases such as `y` in the code above are inaccessible as long as `x` is also visible.

Separation checking applies only to exclusive capabilities and their read-only versions. Any capability extending `SharedCapability` in its type is exempted; the following definitions and rules do not apply to them.

**Definitions:**

 - The _transitive capture set_ `tcs(c)` of a capability `c` with underlying capture set `C` is `c` itself, plus the transitive capture set of `C`.

 - The _transitive capture set_ `tcs(C)` of a capture set C is the union
   of `tcs(c)` for all elements `c` of `C`.

 - Two capture sets _interfere_ if one contains an exclusive capability `x` and the other also contains `x` or contains the read-only capability `x.rd`. Conversely, two capture sets are _separated_ if their transitive capture sets don't interfere.

Separation checks are applied in the following scenarios:

### Checking Applications

When checking a function application `f(e_1, ..., e_n)`, we instantiate each `any` in a formal parameter of `f` to a fresh top capability and compare the argument types with these instantiated parameter types. We then check that the hidden set of each instantiated top capability for an argument `eᵢ` is separated from the capture sets of all the other arguments as well as from the capture sets of the function prefix and the function result. For instance a
call to
```scala sc:nocompile
multiply(a, b, a)
```
would be rejected since `a` appears in the hidden set of the last parameter of multiply, which has type `Matrix^` and also appears in the capture set of the
first parameter.

We do not report a separation error between two sets if a formal parameter's capture set explicitly names a conflicting parameter. For instance, consider a method `seq` to apply two effectful function arguments in sequence. It can be declared as follows:
```scala sc:nocompile
def seq(f: () => Unit; g: () ->{any, f} Unit): Unit =
  f(); g()
```
Here, the `g` parameter explicitly mentions `f` in its potential capture set. This means that the `any` in the same capture set would not need to hide the  first argument, since it already appears explicitly in the same set. Consequently, we can pass the same function twice to `compose` without violating the separation criteria:
```scala sc:nocompile
val r = Ref(1)
val plusOne = r.set(r.get + 1)
seq(plusOne, plusOne)
```
Without the explicit mention of parameter `f` in the capture set of parameter `g` of `seq` we'd get a separation error, since the transitive capture sets of both arguments contain `r` and are therefore not separated.

### Checking Statement Sequences

When a capability `x` is used at some point in a statement sequence, we check that `{x}` is separated from the hidden sets of all previous definitions.

Example:
```scala sc:nocompile
val a: Ref^ = Ref(1)
val b: Ref^ = a
val x = a.get // error
```
Here, the last line violates the separation criterion since it uses in `a.get` the capability `a`, which is hidden by the definition of `b`.
Note that this check only applies when there are explicit top capabilities in play. One could very well write
```scala sc:nocompile
val a: Ref^ = Ref(1)
val b: Ref^{a} = a
val x = a.get // ok
```
One can also drop the explicit type of `b` and leave it to be inferred. That would
not cause a separation error either.
```scala sc:nocompile
val a: Ref^ = Ref(0)
val b = a
val x = a.get // ok
```

### Checking Types

When a type contains top capabilities we check that their hidden sets don't interfere with other parts of the same type.

Example:
```scala sc:nocompile
val b: (Ref^, Ref^) = (a, a)       // error
val c: (Ref^, Ref^{a}) = (a, a)    // error
val d: (Ref^{a}, Ref^{a}) = (a, a) // ok
```
Here, the definition of `b` is in error since the hidden sets of the two `^`s in its type both contain `a`. Likewise, the definition of `c` is in error since the hidden set of the `^` in its type contains `a`, which is also part of a capture set somewhere else in the type. On the other hand, the definition of `d` is legal since there are no hidden sets to check.

### Checking Return Types

When an `any` appears in the return type of a method it means a top capability that is different from what is known at the call site. Separation checking makes sure this is the case. For instance, the following is OK:
```scala sc:nocompile
def newRef(): Ref^ = Ref(1)
```
And so is this:
```scala sc:nocompile
def newRef(): Ref^ =
  val a = Ref(1)
  a
```
But the next definitions would cause a separation error:
```scala sc:nocompile
val a = Ref(1)
def newRef(): Ref^ = a // error
```
The rule is that the hidden set of an `any` in a return type cannot reference exclusive or read-only capabilities defined outside of the function. What about parameters? Here's another illegal version:
```scala sc:nocompile
def incr(a: Ref^): Ref^ =
  a.set(a.get + 1)
  a
```
These needs to be rejected because otherwise we could have set up the following bad example:
```scala sc:nocompile
val a = Ref(1)
val b: Ref^ = incr(a)
```
Here, `b` aliases `a` but does not hide it. If we referred to `a` afterwards we would be surprised to see that the reference has now a value of 2.
Therefore, parameters cannot appear in the hidden sets of result `any`s either, at least not in general. An exception to this rule is described in the next section.

### `fresh` in Function Type Results

While method return types use `any`, function types use `fresh` in their result positions to express that each call yields a result with a distinct capability. As explained in [scoped capabilities](scoped-capabilities.md#expansion-rules-for-function-types), `fresh` in a function result is existentially bound: `() -> Ref^{fresh}` means `() -> ∃fresh. Ref^{fresh}`.

From a separation-checking perspective, `fresh` results are important because they let the checker prove non-aliasing across calls:
```scala sc:nocompile
val mkRef: () -> Ref^{fresh} = () => Ref(1)
val a = mkRef()  // Ref^{fresh₁}
val b = mkRef()  // Ref^{fresh₂}
```
Since `fresh₁` and `fresh₂` are distinct existentials, the capture sets of `a` and `b` don't interfere — they are separated. This would not hold if the function type used `any` in its result instead, since both results would share the same capture-set bound and could therefore alias.

The same hidden-set discipline applies: the hidden set of a result `fresh` cannot contain capabilities from outside the function. For instance:
```scala sc:nocompile
val a = Ref(1)
val bad: () => Ref^{fresh} = () => a  // error
```
Here, `a` is captured by the closure and would need to flow into the result `fresh`. But since `a` is also visible outside the function, this would violate the freshness guarantee — the returned `Ref` would not truly be a new, unaliased capability.

### Consume Parameters

Returning parameters in result `any`'s is safe if the actual argument to the parameter is not used afterwards. We can signal and enforce this pattern by adding a `consume` modifier to a parameter. With that new soft modifier, the following variant of `incr` is legal:
```scala sc:nocompile
def incr(consume a: Ref^): Ref^ =
  a.set(a.get + 1)
  a
```
Here, we increment the value of a reference and then return the same reference while enforcing the condition that the original reference cannot be used afterwards. Then the following is legal:
```scala sc:nocompile
val a1 = Ref(1)
val a2 = incr(a1)
val a3 = incr(a2)
println(a3)
```
Each reference `aᵢ` is unused after it is passed to `incr`. But the following continuation of that sequence would be in error:
```scala sc:nocompile
val a4 = println(a2) // error
val a5 = incr(a1)    // error
```
In both of these assignments we use a capability that was consumed in an argument
of a previous application.

Consume parameters enforce linear access to resources. This can be very useful. As an example, consider Scala's  buffers such as `ListBuffer` or `ArrayBuffer`. We can treat these buffers as if they were purely functional, if we can enforce linear access.

For instance, we can define a function `linearAdd` that adds elements to buffers in-place without violating referential transparency:
```scala sc:nocompile
def linearAdd[T](consume buf: Buffer[T]^, elem: T): Buffer[T]^ =
  buf += elem
```
`linearAdd` returns a fresh buffer resulting from appending `elem` to `buf`. It overwrites `buf`, but that's OK since the `consume` modifier on `buf` ensures that the argument is not used after the call.

### Consume Parameters and Read Accesses

A good way to conceptualize consume is to that it reserves the passed capability beyond the call with the consume parameter. In the previous `Buffer` example, if we do
```scala sc:nocompile
  val buf1 = linearAdd(buf, elem)
```
then the exclusive `buf` capability is reserved and therefore no further accesses to `buf` are possible. This means that `linearAdd` can safely overwrite `buf` by appending `elem` to it.

By the same token, when we pass a read-only capability to a consume parameter, it is
only this capability that is reserved beyond the call. For instance, here is a
method that consumes a read-only buffer:
```scala sc:nocompile
def contents[T](consume buf: Buffer[T]): Int ->{buf.rd} T =
  i => buf(i)
```
The `contents` method takes a read-only buffer and turns it into a function that
produces for each valid index the element of the buffer at that index. Passing a
buffer to `contents` effectively freezes it: Since `buf.rd` is reserved, we cannot
use the exclusive `buf` capability beyond the point of call, so no further appends
are possible. On the other hand, it is possible to read the buffer, and it is also possible
to consume that read capability again in further calls:
```scala sc:nocompile
val buf = Buffer[String]()
val buf1 = linearAdd(buf, "hi") // buf unavailable from here
val c1 = contents(buf1)         // only buf.rd is consumed
val c2 = contents(buf1)         // buf.rd can be consumed repeatedly
```
Note that the only difference between `linearAdd` and `contents` is that `linearAdd`'s consume parameter has type `Buffer[T]^` whereas the
corresponding parameter in `contents` has type `Buffer[T]`. The first type expands
to `Buffer[T]^{any}` whereas the second expands to `Buffer[T]^{any.rd}`.


### Consume Methods

Buffers in Scala's standard library use a single-argument method `+=` instead of a two argument global function like `linearAdd`. We can enforce linearity in this case by adding the `consume` modifier to the method itself.
```scala sc:nocompile
class Buffer[T] extends Mutable:
  consume def +=(x: T): Buffer[T]^ = this // ok
```
`consume` on a method in a `Mutable` class implies `update`, so there's no need to label `+=` separately as an update method. Then we can write
```scala sc:nocompile
val b = Buffer[Int]() += 1 += 2
val c = b += 3
// b cannot be used from here
```
This code is equivalent to functional append with `+`, and is at the same time more efficient since it re-uses the storage of the argument buffer.

## The `freeze` Wrapper

We often want to create a mutable data structure like an array, initialize by assigning to its elements and then return the array as an immutable type that does not
capture any capabilities. This can be achieved using the `freeze` wrapper.

As an example, consider a class `Arr` which is modelled after `Array` and its immutable counterpart `IArr`:

```scala sc:nocompile
class Arr[T: reflect.ClassTag](len: Int) extends Mutable:
  private val arr: Array[T] = new Array[T](len)
  def get(i: Int): T = arr(i)
  update def update(i: Int, x: T): Unit = arr(i) = x
type IArr[T] = Arr[T]^{}
```

The `freeze` wrapper allows us to go from an `Arr` to an `IArr`, safely:
```scala sc:nocompile
import caps.freeze

val f: IArr[String] =
  val a = Arr[String](2)
  a(0) = "hello"
  a(1) = "world"
  freeze(a)
```
The `freeze` method is defined in `caps` like this:
```scala sc:nocompile
def freeze(consume x: Mutable): x.type = x
```
It consumes a value of `Mutable` type with arbitrary capture set (since any capture set conforms to the implied `{any.rd}`). The actual signature of
`consume` declares that `x.type` is returned, but the actual return type after capture checking is special. Instead of `x.type` it is the underlying `Mutable` type with its top-level capture set
mapped to `{}`. Applications of `freeze` are safe only if separation checking is enabled.

