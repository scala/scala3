---
layout: doc-page
title: "Replacing Implicits"
---

The previous pages describe a new, high-level syntax for implicit definitions, parameters, function literals, and function types. With the exception of context parameters

These idioms can by-and-large be mapped to existing implicits. The only exception concerns context parameters which give genuinely more freedom in the way parameters can be organized. The new idioms are preferable to existing implicits since they are both more concise and better behaved. The better expressiveness comes at a price, however, since it leaves us with two related constructs: new style instance definitions and context parameters and traditional implicits. This page discusses what would be needed to get rid of `implicit` entirely.

The contents of this page are more tentative than the ones of the previous pages. The concepts described in the previous pages are useful independently whether the changes on this page are adopted.

The current Dotty implementation implements the new concepts described on this page (implicit as a modifier and the summon method), but it does not remove any of the old-style implicit constructs. It cannot do this since support
for old-style implicits is an essential part of the common language subset of Scala 2 and Scala 3.0. Any deprecation and subsequent removal of these constructs would have to come later, in a version following 3.0. The `implicit` modifier can be removed from the language at the end of this development, if it happens.

## `instance` As A Modifier.

`instance` can be used as a modifier for `val` and `def` definitions. Examples:
```scala
instance val symDeco: SymDeco
instance val symDeco: SymDeco = compilerSymOps
instance val ctx = localCtx
instance def f[T]: C[T] = new C[T]
instance def g with (ctx: Context): D = new D(ctx)
```
The `instance` modifier must be followed directly by `val` or `def`; no other intervening modifiers are permitted.
When used as a modifier, `instance` generally has the same meaning as the current `implicit` modifier, with the following exceptions:

 1. `instance def` definitions can only have context parameters in `with` clauses. Old style `implicit` parameters are not supported.
 2. `instance` cannot be used to define an implicit conversion or an implicit class.


## Replaced: Implicit Conversions

Implicit conversions using the `implicit def` syntax are no longer needed, since they
can be expressed as instances of the `scala.Conversion` class: This class is defined in package `scala` as follows:
```scala
abstract class Conversion[-T, +U] extends (T => U)
```
For example, here is an implicit conversion from `String` to `Token`:
```scala
instance of Conversion[String, Token] {
  def apply(str: String): Token = new KeyWord(str)
}
```
The fact that this syntax is more verbose than simple implicit defs could be a welcome side effect since it might dampen any over-enthusiasm for defining implicit conversions.

## Dropped: Implicit Classes

Most use cases of implicit classes are already covered by extension methods. For the others, one can always fall back to a pair of a regular class and a `Conversion` instance.

## Summoning an Instance

Besides `implicit`, there is also `implicitly`, a method defined in `Predef` that computes an implicit value for a given type. We propose to rename this operation to `summon`. So `summon[T]` summons an instance of `T`, in the same way as `implicitly[T]` did. The definition of `summon` is straightforward:
```scala
def summon[T] with (x: T) = x
```

## Syntax

The syntax changes for this page are summarized as follows:
```
InstanceDef     ::=  ...
                  |  ‘val’ PatDef
                  |  ‘def’ MethodDef
```
In addition, the `implicit` modifier is removed together with all [productions]((http://dotty.epfl.ch/docs/internals/syntax.html) that reference it.

## Further Reading

Here is the [original proposal](./discussion/motivation.html) that makes the case for the changes described in these pages.
