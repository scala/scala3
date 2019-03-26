---
layout: doc-page
title: "Implicit Function Types - More Details"
---

Initial implementation in (#1775)[https://github.com/lampepfl/dotty/pull/1775].

## Syntax

    Type              ::=  [`implicit'] FunArgTypes `=>' Type
                        |  HkTypeParamClause `=>' Type
                        |  InfixType
    Expr              ::=  [`implicit'] FunParams `=>' Expr
    BlockResult       ::=  [`implicit'] FunParams `=>' Block
                        |  Expr1

Implicit function types associate to the right, e.g.
`implicit S ⇒ implicit T ⇒ U` is the same as `implicit S ⇒ (implicit T ⇒ U)`.

## Implementation

Implicit function types are shorthands for class types that define `apply`
methods with implicit parameters. Specifically, the `N`-ary function type
`implicit T1, ..., TN ⇒ R` is a shorthand for the class type
`ImplicitFunctionN[T1 , ... , TN, R]`. Such class types are defined in the
Scala library for `N` between 1 and 22 as follows.

    package scala
    trait ImplicitFunctionN[-T1 , ... , -TN, +R] {
      def apply(implicit x1: T1 , ... , xN: TN): R
    }

Anonymous implicit functions `implicit (x1: T1, ..., xn: Tn) => e` map
implicit parameters `xi` of types `Ti` to a result given by expression `e`.
The scope of each implicit parameter `xi` is `e`. Implicit parameters must
have pairwise distinct names.

If the expected type of the anonymous implicit function is of the form
`scala.ImplicitFunctionN[S1, ..., Sn, R]`, the expected type of `e` is `R` and
the type `Ti` of any of the parameters `xi` can be omitted, in which case `Ti
= Si` is assumed. If the expected type of the anonymous implicit function is
some other type, all implicit parameter types must be explicitly given, and
the expected type of `e` is undefined. The type of the anonymous implicit
function is `scala.ImplicitFunctionN[S1, ...,Sn, T]`, where `T` is the widened
type of `e`. `T` must be equivalent to a type which does not refer to any of
the implicit parameters `xi`.

The anonymous implicit function is evaluated as the instance creation
expression:

    new scala.ImplicitFunctionN[T1, ..., Tn, T] {
      def apply(implicit x1: T1, ..., xn: Tn): T = e
    }

In the case of a single untyped implicit parameter, `implicit (x) => e` can be
abbreviated to `implicit x => e`. If an anonymous implicit function `implicit
(x: T) => e` with a single typed parameter appears as the result expression of
a block, it can be abbreviated to `implicit x: T => e`

A implicit parameter may also be a wildcard represented by an underscore `_`. In
that case, a fresh name for the parameter is chosen arbitrarily.

Note: The closing paragraph of the [Anonymous Functions section](https://www
.scala-lang.org/files/archive/spec/2.12/06-expressions.html#anonymous-
functions) of the Scala 2.12 is subsumed by implicit function types and should
be removed.

Anonymous implicit functions `implicit (x1: T1, ..., xn: Tn) => e` are
automatically inserted around any expression `e` whose expected type is
`scala.ImplicitFunctionN[T1, ..., Tn, R]`. This is analogous to the automatic
insertion of `scala.Function0` around expression in by-name argument position.

Implicit functions generalize to `N > 22` in the same way that functions do,
see [the corresponding
documentation](https://dotty.epfl.ch/docs/reference/dropped-features/limit22.html).

## Examples

See the section on Expressiveness from [Simplicitly: foundations and
applications of implicit function
types](https://dl.acm.org/citation.cfm?id=3158130). I've extracted it in [this
Gist](https://gist.github.com/OlivierBlanvillain/234d3927fe9e9c6fba074b53a7bd9592), it might easier to access than the pdf.

### Type Checking

After desugaring no additional typing rules are required for implicit function
types.
