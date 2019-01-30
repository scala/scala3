---
layout: doc-page
title: "Replacing Implicits"
---

The previous pages describe a new, high-level syntax for implicit definitions, parameters, function literals, and function types.
These idioms can by-and-large be mapped to existing implicits. The only exception concerns context parameters which give genuinely more freedom in the way parameters can be organized. The new idioms are preferable to existing implicits since they are both more concise and better behaved. The better expressiveness comes at a price, however, since it leaves us with both the new and the old way to express implicits. This page discusses what would be needed to get rid of all existing uses of `implicit` as a modifier.

The current Dotty implementation implements the new concepts described on this page (alias instances and the summon method), but it does not remove any of the old-style implicit constructs. It cannot do this since support
for old-style implicits is an essential part of the common language subset of Scala 2 and Scala 3.0. Any deprecation and subsequent removal of these constructs would have to come later, in a version following 3.0. The `implicit` modifier can be removed from the language at the end of this development, if it happens.

## Add: Alias Instances

To replace implicit vals and defs (both abstract and concrete), we need one way to
"lift" an existing value to become an implicit instance for a type. This is achieved
by an alias instance, which creates an instance that is equal to some expression.
```scala
implicit ctx for ExecutionContext = currentThreadPool().context
```
Here, we create an implicit `ctx` of type `ExecutionContext` that resolves to the
right hand side `currentThreadPool().context`. Each time an implicit of `ExecutionContext`
is demanded, the result of evaluating the right-hand side expression is returned. The  definition is equivalent to the following implicit definition in Scala 2:
```scala
final implicit def ctx: ExecutionContext = currentThreadPool().context
```
Implicit aliases may be anonymous, e.g.
```scala
implicit for Position = enclosingTree.position
```
An implicit alias can have type and context parameters just like any other implicit definition, but it can only implement a single type.

## Drop: Implicit Conversions

Implicit conversions using the `implicit def` syntax are no longer needed, since they
can be expressed as instances of the `scala.Conversion` class.

## Drop: Implicit Classes

Most use cases of implicit classes are already covered by extension methods. For the others, one can always fall back to a pair of a regular class and a `Conversion` instance.

## Drop: Implicit As A Modifier

 - Old-style implicit parameters are replaced by `given` parameters.
 - Implicit function types `implicit T => U` are written `given T => U`
 - Implicit closures `implicit x => e` are written `given x => e`
 - All remaining implicit `val` and `def` definition are replaced by normal
   `val` or `def` definitions and implicit aliases.=

## Syntax

The syntax changes for this page are summarized as follows:
```
InstanceBody     ::=  ...
                   |  ‘for’ Type ‘=’ Expr
```
In addition, the `implicit` modifier is removed together with all [productions]((http://dotty.epfl.ch/docs/internals/syntax.html) that reference it.
