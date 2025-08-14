---
layout: doc-page
title: "Scoped Caps"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/scoped-caps.html
---

## Scoped Universal Capabilities

When discussing escape checking, we referred to a scoping discipline. That is, capture sets can contain only capabilities that are visible at the point where the set is defined. But that raises the question where a universal capability `cap` is defined? In fact, what is written as the top type `cap` can mean different capabilities, depending on scope. Usually a `cap` refers to a universal capability defined in the scope where the `cap` appears.

Special rules apply to `cap`s in method and function parameters and results. For example, take this method:

```scala
  def makeLogger(fs: FileSystem^): Logger^ = new Logger(fs)
```
This creates a `Logger` that captures `fs`.
We could have been more specific in specifying `Logger^{fs}` as the return type of makeLogger, but the current definition is also valid, and might be preferable if we want to hide details what the returned logger captures. If we write it as above then certainly the implied `cap` in the return type of `Logger` should be able subsume the capability `fs`. This means that this `cap` has to be defined in a scope in which
`fs` is visible.

In logic, the usual way to achieve this scoping is with an existential binder. We can express the type of `makeLogger` like this:
```scala
makeLogger: (fs: ∃cap₁.FileSystem^{cap₁}): ∃cap₂. Logger^{cap₂}
```
In words: `makeLogger` takes a parameter `fs` of type `Filesystem` capturing _some_ universal capability `cap` and returns a `Logger` capturing some other (possibly different) universal `cap`.

We can also turn the existential in the function parameter to a universal "forall"
in the function itself. In that alternative notation, the type of makeLogger would read like this:
```scala
makeLogger: ∀cap₁.(fs: FileSystem^{cap₁}): ∃cap₂. Logger^{cap₂}
```
There's a connection with [capture polymorphism](polymorphism.md) here. `cap`s in function parameters behave like additional
capture parameters that can be instantiated at the call site to arbitrary capabilities.

The conventions for method types carry over to function types. A function type
```scala
  (x: T) -> U^
```
is interpreted as having an existentially bound `cap` in the result, like this
```scala
  (x: T) -> ∃cap.U^{cap}
```
The same rules hold for the other kinds of function arrows, `=>`, `?->`, and `?=>`. So `cap` can in this case
subsume the function parameter `x` since it is locally bound in the function result.

However, the expansion of `cap` into an existentially bound variable only applies to functions that use
the dependent function style syntax, with explicitly named parameters. Parametric functions such as
`A => B^` or `(A₍, ..., Aₖ) -> B^` don't bind their result cap in an existential quantifier.
For instance, the function
```scala
  (x: A) -> B -> C^
```
is interpreted as
```scala
  (x: A) -> ∃cap.B -> C^{cap}
```
In other words, existential quantifiers are only inserted in results of function arrows that follow an explicitly named parameter list.

To summarize:

  - If a function result type follows a named parameter list and contains covariant occurrences of `cap`,
    we replace these occurrences with a fresh existential variable which
    is bound by a quantifier scoping over the result type.
  - If a function parameter type contains covariant occurrences of `cap`, we replace these occurrences with
    a fresh existential variable scoping over the parameter type.
  - Occurrences of `cap` elsewhere are not translated. They can be seen as representing an existential in the
    scope of the definition in which they appear.

**Examples:**

 - `A => B` is an alias type that expands to `A ->{cap} B`, therefore
   `(x: T) -> A => B` expands to `(x: T) -> ∃cap.(A ->{cap} B)`.

 - `(x: T) -> Iterator[A => B]` expands to `() -> ∃cap.Iterator[A ->{cap} B]`
<!--
 - If we define `type Fun[T] = (y: B) -> T`, then `(x: A) -> Fun[C^]` expands to
   `(y: B) -> ∃cap. Fun[C^{cap}]`, which dealiases to `(x: A) -> ∃cap.(y: B) -> C^{cap}`.
   This demonstrates how aliases can be used to force existential binders to be in some specific outer scope.

**Typing Rules:**

 - When we typecheck the body of a method, any covariant occurrences of `cap` in the result type are bound with a fresh existential.
 - Conversely, when we typecheck the application of a function or method,
  with an existential result type `Exists ex.T`, the result of the application is `T` where every occurrence of the existentially bound
  variable `ex` is replaced by `cap`.
-->

<!--
## Reach Capabilities

Say you have a method `f` that takes an impure function argument which gets stored in a `var`:
```scala
def f(op: A => B)
  var x: A ->{op} B = op
  ...
```
This is legal even though `var`s cannot have types with `cap` or existential capabilities. The trick is that the type of the variable `x`
is not `A => B` (this would be rejected), but is the "narrowed" type
`A ->{op} B`. In other words, all capabilities retained by values of `x`
are all also referred to by `op`, which justifies the replacement of `cap` by `op`.

A more complicated situation is if we want to store successive values
held in a list. Example:
```scala
def f(ops: List[A => B])
  var xs = ops
  var x: ??? = xs.head
  while xs.nonEmpty do
    xs = xs.tail
    x = xs.head
  ...
```
Here, `x` cannot be given a type with an `ops` capability. In fact, `ops` is pure, i.e. it's capture set is empty, so it cannot be used as the name of a capability. What we would like to express is that `x` refers to
any operation "reachable" through `ops`. This can be expressed using a
_reach capability_ `ops*`.
```scala
def f(ops: List[A => B])
  var xs = ops
  var x: A ->{ops*} B = xs.head
  ...
```
Reach capabilities take the form `x*` where `x` is syntactically a regular capability. If `x: T` then `x*` stands for any capability that appears covariantly in `T` and that is accessed through `x`. The least supertype of this capability is the set of all capabilities appearing covariantly in `T`.
-->