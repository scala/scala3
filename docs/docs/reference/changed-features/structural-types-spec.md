---
layout: doc-page
title: "Programmatic Structural Types - More Details"
---

## Syntax

```
SimpleType    ::= ... | Refinement
Refinement    ::= ‘{’ RefineStatSeq ‘}’
RefineStatSeq ::=  RefineStat {semi RefineStat}
RefineStat    ::= ‘val’ VarDcl | ‘def’ DefDcl | ‘type’ {nl} TypeDcl
```

## Implementation of structural types

The standard library defines a universal marker trait
[`scala.Selectable`](https://github.com/lampepfl/dotty/blob/master/library/src/scala/Selectable.scala):

```scala
trait Selectable extends Any
```

An implementation of `Selectable` that relies on [Java reflection](https://www.oracle.com/technical-resources/articles/java/javareflection.html) is
available in the standard library: `scala.reflect.Selectable`. Other
implementations can be envisioned for platforms where Java reflection
is not available.

Implementations of `Selectable` have to make available one or both of
the methods `selectDynamic` and `applyDynamic`. The methods could be members of the `Selectable` implementation or they could be extension methods.

The `selectDynamic` method takes a field name and returns the value associated with that name in the `Selectable`.
It should have a signature of the form:

```scala
def selectDynamic(name: String): T
```

Often, the return type `T` is `Any`.

Unlike `scala.Dynamic`, there is no special meaning for an `updateDynamic` method.
However, we reserve the right to give it meaning in the future.
Consequently, it is recommended not to define any member called `updateDynamic` in `Selectable`s.

The `applyDynamic` method is used for selections that are applied to arguments. It takes a method name and possibly `Class`es representing its parameters types as well as the arguments to pass to the function.
Its signature should be of one of the two following forms:

```scala
def applyDynamic(name: String)(args: Any*): T
def applyDynamic(name: String, ctags: Class[?]*)(args: Any*): T
```

Both versions are passed the actual arguments in the `args` parameter. The second version takes in addition a vararg argument of `java.lang.Class`es that identify the method's parameter classes. Such an argument is needed
if `applyDynamic` is implemented using Java reflection, but it could be
useful in other cases as well. `selectDynamic` and `applyDynamic` can also take additional context parameters in using clauses. These are resolved in the normal way at the callsite.

Given a value `v` of type `C { Rs }`, where `C` is a class reference
and `Rs` are structural refinement declarations, and given `v.a` of type `U`, we consider three distinct cases:

- If `U` is a value type, we map `v.a` to:
  ```scala
  v.selectDynamic("a").asInstanceOf[U]
  ```

- If `U` is a method type `(T11, ..., T1n)...(TN1, ..., TNn): R` and it is not a dependent method type, we map `v.a(a11, ..., a1n)...(aN1, ..., aNn)` to:
  ```scala
  v.applyDynamic("a")(a11, ..., a1n, ..., aN1, ..., aNn)
    .asInstanceOf[R]
  ```
  If this call resolves to an `applyDynamic` method of the second form that takes a `Class[?]*` argument, we further rewrite this call to
  ```scala
  v.applyDynamic("a", c11, ..., c1n, ..., cN1, ... cNn)(
    a11, ..., a1n, ..., aN1, ..., aNn)
    .asInstanceOf[R]
  ```
   where each `c_ij` is the literal `java.lang.Class[?]` of the type of the formal parameter `Tij`, i.e., `classOf[Tij]`.

- If `U` is neither a value nor a method type, or a dependent method
  type, an error is emitted.

Note that `v`'s static type does not necessarily have to conform to `Selectable`, nor does it need to have `selectDynamic` and `applyDynamic` as members. It suffices that there is an implicit
conversion that can turn `v` into a `Selectable`, and the selection methods could also be available as
[extension methods](../contextual/extension-methods.md).

## Limitations of structural types

- Dependent methods cannot be called via structural call.
- Overloaded methods cannot be called via structural call.
- Refinements do not handle polymorphic methods.

## Differences with Scala 2 structural types

- Scala 2 supports structural types by means of Java reflection. Unlike
  Scala 3, structural calls do not rely on a mechanism such as
  `Selectable`, and reflection cannot be avoided.
- In Scala 2, structural calls to overloaded methods are possible.
- In Scala 2, mutable `var`s are allowed in refinements. In Scala 3,
  they are no longer allowed.


## Context

For more information, see [Rethink Structural Types](https://github.com/lampepfl/dotty/issues/1886).
