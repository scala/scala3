---
layout: doc-page
title: "Implied Instances"
---

Implied instance definitions define "canonical" values of given types
that serve for synthesizing arguments to [inferable parameters](./inferable-params.html). Example:

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

implied IntOrd for Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

implied ListOrd[T] given (ord: Ord[T]) for Ord[List[T]] {
  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match {
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = ord.compare(x, y)
      if (fst != 0) fst else xs1.compareTo(ys1)
  }
}
```
This code defines a trait `Ord` and two implied instance definitions. `IntOrd` defines
an implied instance for the type `Ord[Int]` whereas `ListOrd[T]` defines implied
instances of `Ord[List[T]]` for all types `T` that come with an implied `Ord[T]` instance themselves.
The `given` clause in `ListOrd` defines an [inferable parameter](./inferable-params.html).
Inferable parameters are further explained in the next section.

## Anonymous Implied Instances

The name of an implied instance can be left out. So the implied instance definitions
of the last section can also be expressed like this:
```scala
implied for Ord[Int] { ... }
implied [T] given (ord: Ord[T]) for Ord[List[T]] { ... }
```
If the name of an instance is missing, the compiler will synthesize a name from
the type(s) in the `for` clause.

## Implied Alias Instances

An implied alias instance defines an implied instance that is equal to some expression. E.g., assuming a global method `currentThreadPool` returning a value with a member `context`, one could define:
```scala
implied ctx for ExecutionContext = currentThreadPool().context
```
This creates an implied instance `ctx` of type `ExecutionContext` that resolves to the right hand side `currentThreadPool().context`. Each time an implied instance of `ExecutionContext` is demanded, the result of evaluating the right-hand side expression is returned.

Alias instances may be anonymous, e.g.
```scala
implied for Position = enclosingTree.position
```
An implied alias instance can have type and context parameters just like any other implied instance definition, but it can only implement a single type.

## Syntax

Here is the new syntax of implied instance definitions, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |  ‘implied’ InstanceDef
InstanceDef      ::=  [id] InstanceParams InstanceBody
InstanceParams   ::=  [DefTypeParamClause] {GivenParamClause}
GivenParamClause ::=  ‘given’ (‘(’ [DefParams] ‘)’ | GivenTypes)
InstanceBody     ::=  [‘for’ ConstrApp {‘,’ ConstrApp }] [TemplateBody]
                   |  ‘for’ Type ‘=’ Expr
GivenTypes       ::=  AnnotType {‘,’ AnnotType}
ConstrApp        ::=  SimpleConstrApp
                   |  ‘(’ SimpleConstrApp {‘given’ (PrefixExpr | ParArgumentExprs)} ‘)’
```
The identifier `id` can be omitted only if either the `for` part or the template body is present.
If the `for` part is missing, the template body must define at least one extension method.
