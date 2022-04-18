---
layout: doc-page
title: "Context Functions - More Details"
movedTo: https://docs.scala-lang.org/scala3/reference/contextual/context-functions-spec.html
---

## Syntax

```ebnf
Type              ::=  ...
                    |  FunArgTypes ‘?=>’ Type ;
Expr              ::=  ...
                    |  FunParams ‘?=>’ Expr ;
```

Context function types associate to the right, e.g.
`S ?=> T ?=> U` is the same as `S ?=> (T ?=> U)`.

## Implementation

Context function types are shorthands for class types that define `apply`
methods with context parameters. Specifically, the `N`-ary function type

`T1, ..., TN ?=> R` is a shorthand for the class type
`ContextFunctionN[T1, ..., TN, R]`. Such class types are assumed to have the following definitions, for any value of `N >= 1`:

```scala
package scala
trait ContextFunctionN[-T1, ..., -TN, +R]:
  def apply(using x1: T1, ..., xN: TN): R
```

Context function types erase to normal function types, so these classes are
generated on the fly for typechecking, but not realized in actual code.

Context function literals `(x1: T1, ..., xn: Tn) ?=> e` map
context parameters `xi` of types `Ti` to the result of evaluating the expression `e`.
The scope of each context parameter `xi` is `e`. The parameters must have pairwise distinct names.

If the expected type of the context function literal is of the form
`scala.ContextFunctionN[S1, ..., Sn, R]`, the expected type of `e` is `R` and
the type `Ti` of any of the parameters `xi` can be omitted, in which case `Ti
= Si` is assumed. If the expected type of the context function literal is
some other type, all context parameter types must be explicitly given, and the expected type of `e` is undefined.
The type of the context function literal is `scala.ContextFunctionN[S1, ...,Sn, T]`, where `T` is the widened
type of `e`. `T` must be equivalent to a type which does not refer to any of
the context parameters `xi`.

The context function literal is evaluated as the instance creation expression

```scala
new scala.ContextFunctionN[T1, ..., Tn, T]:
  def apply(using x1: T1, ..., xn: Tn): T = e
```

A context parameter may also be a wildcard represented by an underscore `_`. In that case, a fresh name for the parameter is chosen arbitrarily.

**Note:** The closing paragraph of the
[Anonymous Functions section](https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html#anonymous-functions)
of Scala 2.13 is subsumed by context function types and should be removed.

Context function literals `(x1: T1, ..., xn: Tn) ?=> e` are
automatically created for any expression `e` whose expected type is
`scala.ContextFunctionN[T1, ..., Tn, R]`, unless `e` is
itself a context function literal. This is analogous to the automatic
insertion of [`scala.Function0`](https://scala-lang.org/api/3.x/scala/Function0.html) around expressions in by-name argument position.

Context function types generalize to `N > 22` in the same way that function types do, see [the corresponding
documentation](../dropped-features/limit22.md).

## Examples

See the section on Expressiveness from [Simplicitly: foundations and
applications of implicit function
types](https://dl.acm.org/citation.cfm?id=3158130).

### Type Checking

After desugaring no additional typing rules are required for context function types.
