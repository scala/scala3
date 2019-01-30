---
layout: doc-page
title: "Inferred Instances"
---

Inferred instance definitions define "canonical" values of given types
that serve for synthesizing arguments to [inferable parameters](./inferable-params.html). Example:

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

inferred IntOrd for Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

inferred ListOrd[T: Ord] for Ord[List[T]] {
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
This code defines a trait `Ord` and two inferred instance definitions. `IntOrd` defines
an inferred instance for the type `Ord[Int]` whereas `ListOrd` defines inferred
instances of `Ord[List[T]]` for any type `T` that comes with an inferred `Ord` instance itself.
The `given` clause in `ListOrd`  defines an [implicit parameter](./implicit-params.html).
Implicit parameters are further explained in the next section.

## Anonymous Inferred Instances

The name of an inferred instance can be left out. So the inferred instance definitions
of the last section can also be expressed like this:
```scala
inferred for Ord[Int] { ... }
inferred [T: Ord] for Ord[List[T]] { ... }

If the name of an instance is missing, the compiler will synthesize a name from
the type(s) in the `for` clause.

## Inferred Alias Instances

An inferred alias instance creates an inferred instance that is equal to
some expression. E.g.,
```
inferred ctx for ExecutionContext = currentThreadPool().context
```
Here, we create an inferred instance `ctx` of type `ExecutionContext` that resolves to the
right hand side `currentThreadPool().context`. Each time an inferred instance of `ExecutionContext`
is demanded, the result of evaluating the right-hand side expression is returned. The instance definition is equivalent to the following implicit definition:
```
final implicit def ctx: ExecutionContext = currentThreadPool().context
```
Alias instances may be anonymous, e.g.
```
inferred for Position = enclosingTree.position
```
An inferred alias instance can have type and context parameters just like any other inferred instance definition, but it can only implement a single type.

## Syntax

Here is the new syntax of inferred instance definitions, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |  ‘inferred’ InstanceDef
InstanceDef      ::=  [id] InstanceParams InstanceBody
InstanceParams   ::=  [DefTypeParamClause] {InferParamClause}
InferParamClause ::=  ‘given’ (‘(’ [DefParams] ‘)’ | ContextTypes)
InstanceBody     ::=  [‘for’ ConstrApp {‘,’ ConstrApp }] [TemplateBody]
                   |  ‘for’ Type ‘=’ Expr
ContextTypes     ::=  RefinedType {‘,’ RefinedType}
```
The identifier `id` can be omitted only if either the `for` part or the template body is present.
If the `for` part is missing, the template body must define at least one extension method.
