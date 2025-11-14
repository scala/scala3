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
`consume` on a method in a `Mutable` class implies `update`, so there's no need to label `+=` separately as an update method. Then we can write
```scala
val b = Buffer[Int]() += 1 += 2
val c = b += 3
// b cannot be used from here
```
This code is equivalent to functional append with `+`, and is at the same time more efficient since it re-uses the storage of the argument buffer.

