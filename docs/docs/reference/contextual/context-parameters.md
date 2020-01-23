---
layout: doc-page
title: "Context Parameters"
---

Functional programming tends to express most dependencies as simple function parameterization.
This is clean and powerful, but it sometimes leads to functions that take many parameters where the same value is passed over and over again in long call chains to many
functions. Context parameters can help here since they enable the compiler to synthesize
repetitive arguments instead of the programmer having to write them explicitly.

For example, with the [given instances](./givens.md) defined previously,
a maximum function that works for any arguments for which an ordering exists can be defined as follows:
```scala
def max[T](x: T, y: T) with (ord: Ord[T]) : T =
  if ord.compare(x, y) < 0 then y else x
```
Here, `ord` is a _context parameter_ introduced with a `with` clause.
The `max` method can be applied as follows:
```scala
max(2, 3).with(intOrd)
```
The `.with(intOrd)` part passes `intOrd` as an argument for the `ord` parameter. But the point of
context parameters is that this argument can also be left out (and it usually is). So the following
applications are equally valid:
```scala
max(2, 3)
max(List(1, 2, 3), Nil)
```
Formatting hint: For legibility it is recommended to always leave one space after a `with` clause before following it with another lexeme.

## Anonymous Context Parameters

In many situations, the name of a context parameter need not be
mentioned explicitly at all, since it is used only in synthesized arguments for
other context parameters. In that case one can avoid defining a parameter name
and just provide its type. Example:
```scala
def maximum[T](xs: List[T]) with Ord[T] : T =
  xs.reduceLeft(max)
```
`maximum` takes a context parameter of type `Ord` only to pass it on as an
inferred argument to `max`. The name of the parameter is left out.

Generally, context parameters may be defined either as a full parameter list `(p_1: T_1, ..., p_n: T_n)` or just as a sequence of types `T_1, ..., T_n`. Vararg parameters are not supported in with clauses.

**Note:** According to the rules above, a `with` clause like `(A, B)` consisting of types in parentheses defines a context parameter of tuple type. But by analogy with `(a: A, b: B)` one might assume that it defines two context parameters
of type `A` and `B` instead. To avoid confusion, the compiler could issue a warning in this case that clarifies the meaning. If a context parameter of tuple type is in fact intended, the warning can be avoided by switching to a named context parameter,
e.g. `(ab: (A, B))` or enclosing the tuple in an extra set of parentheses, e.g. `((A, B))`.

## Inferring Complex Arguments

Here are two other methods that have a context parameter of type `Ord[T]`:
```scala
def descending[T] with (asc: Ord[T]) : Ord[T] = new Ord[T] {
  def compare(x: T, y: T) = asc.compare(y, x)
}

def minimum[T](xs: List[T]) with Ord[T] =
  maximum(xs).with(descending)
```
The `minimum` method's right hand side passes `descending` as an explicit argument to `maximum(xs)`.
With this setup, the following calls are all well-formed, and they all normalize to the last one:
```scala
minimum(xs)
maximum(xs).with(descending)
maximum(xs).with(descending.with(listOrd))
maximum(xs).with(descending.with(listOrd.with(intOrd)))
```

## Multiple With Clauses

There can be several `with` clauses in a definition. Example:
```scala
def f(u: Universe) with (ctx: u.Context) with (s: ctx.Symbol, k: ctx.Kind) = ...
```
Multiple with clauses are matched left-to-right in applications. Example:
```scala
object global extends Universe { type Context = ... }
given ctx  as global.Context { type Symbol = ...; type Kind = ... }
given sym  as ctx.Symbol
given kind as ctx.Kind
```
Then the following calls are all valid (and normalize to the last one)
```scala
f(global)
f(global).with(ctx)
f(global).with(ctx).with(sym, kind)
```
But `f(global).with(sym, kind)` would give a type error.

`with` clauses can be freely interspersed with normal parameters, but a normal parameter clause cannot
directly follow a `with` clause consisting only of types outside parentheses. So the following is illegal:
```scala
def f with A, B (x: C) = ...
```
But the following variants are valid:
```scala
def g with A, B with (x: C) = ...
def h with (A, B) (x: C) = ...
```

## Summoning Instances

The method `summon` in `Predef` returns the given of a specific type. For example,
the given instance for `Ord[List[Int]]` is produced by
```scala
summon[Ord[List[Int]]]  // reduces to listOrd.with(intOrd)
```
The `summon` method is simply defined as the (non-widening) identity function over a context parameter.
```scala
def summon[T] with (x: T): x.type = x
```

## Syntax

Here is the new syntax of parameters and arguments seen as a delta from the [standard context free syntax of Scala 3](../../internals/syntax.md).
```
ClsParamClauses     ::=  ...
                      |  ClsParamClause ClsParamClauses
                      |  ClsParamClauses1
ClsParamClauses1    ::=  WithClsParamClause ClsParamClauses
                      |  AnnotTypes ClsParamClauses1
DefParamClauses     ::=  ...
                      |  DefParamClause DefParamClauses
                      |  DefParamClauses1
DefParamClauses1    ::=  WithParamClause DefParamClauses
                      |  AnnotTypes DefParamClauses1
WithClsParamClause  ::=  ‘with’ (‘(’ (ClsParams | Types) ‘)’ | AnnotTypes)
WithParamClause     ::=  ‘with’ (‘(’ (DefParams | Types) ‘)’ | AnnotTypes)
Types               ::=  Type {‘,’ Type}
AnnotTypes          ::=  AnnotType {‘,’ AnnotType}

SimpleExpr          ::=  ...
                      |  SimpleExpr ContextArguments
ParArgumentExprss   ::=  {ParArgumentExprs | ContextArguments}
ContextArguments    ::=  ‘.’ ‘with’ ArgumentExprs

```
