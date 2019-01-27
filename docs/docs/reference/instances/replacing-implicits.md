---
layout: doc-page
title: "Replacing Implicits"
---

The previous pages describe a new, high-level syntax for implicit definitions, parameters, function literals, and function types. With the exception of context parameters

These idioms can by-and-large be mapped to existing implicits. The only exception concerns context parameters which give genuinely more freedom in the way parameters can be organized. The new idioms are preferable to existing implicits since they are both more concise and better behaved. The better expressiveness comes at a price, however, since it leaves us with two related constructs: new style instance definitions and context parameters and traditional implicits. This page discusses what would be needed to get rid of `implicit` entirely.

The contents of this page are more tentative than the ones of the previous pages. The concepts described in the previous pages are useful independently whether the changes on this page are adopted.

The current Dotty implementation implements the new concepts described on this page (alias instances and the summon method), but it does not remove any of the old-style implicit constructs. It cannot do this since support
for old-style implicits is an essential part of the common language subset of Scala 2 and Scala 3.0. Any deprecation and subsequent removal of these constructs would have to come later, in a version following 3.0. The `implicit` modifier can be removed from the language at the end of this development, if it happens.

## Alias Instances

An alias instance creates an instance that is equal to some expression.
```
instance ctx of ExecutionContext = currentThreadPool().context
```
Here, we create an instance `ctx` of type `ExecutionContext` that resolves to the
right hand side `currentThreadPool().context`. Each time an instance of `ExecutionContext`
is demanded, the result of evaluating the right-hand side expression is returned. The instance definition is equivalent to the following implicit definition:
```
final implicit def ctx: ExecutionContext = currentThreadPool().context
```
Alias instances may be anonymous, e.g.
```
instance of Position = enclosingTree.position
```
An alias instance can have type and context parameters just like any other instance definition, but it can only implement a single type.

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
InstanceBody     ::=  ...
                   |  ‘of’ Type ‘=’ Expr
```
In addition, the `implicit` modifier is removed together with all [productions]((http://dotty.epfl.ch/docs/internals/syntax.html) that reference it.

## Further Reading

Here is the [original proposal](./discussion/motivation.html) that makes the case for the changes described in these pages.
