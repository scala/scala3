---
layout: doc-page
title: "Dependent Function Types - More Details"
---

Initial implementation in [#3464](https://github.com/lampepfl/dotty/pull/3464)

## Syntax

    FunArgTypes       ::=  InfixType
                        |  ‘(’ [ FunArgType {‘,’ FunArgType } ] ‘)’
                        |  ‘(’ TypedFunParam {',' TypedFunParam } ‘)’
    TypedFunParam     ::=  id ‘:’ Type

Dependent function types associate to the right, e.g.
`(s: S) ⇒ (t: T) ⇒ U` is the same as `(s: S) ⇒ ((t: T) ⇒ U)`.

## Implementation

Dependent function types are shorthands for class types that define `apply`
methods with a dependent result type.Dependent function types desugar to
refinement types of `scala.FunctionN`. A dependent functon type
`(x1: K1, ..., xN: KN) => R` of arity `N` translates to

    FunctionN[K1, ..., Kn, R'] {
      def apply(x1: K1, ..., xN: KN): R
    }

where the result type parameter `R'` is the least upper approximation of the
precise result type `R` without any referance to value parameters `x1, ..., xN`.

The syntax and sementics of anonymous dependent functions is identical to the
one of regular functions. Eta expansion is naturaly generalized to produce
dependent function types for methods with dependent result types.

Dependent functions can be implicit, and generalize to arity `N > 22` in the
same way that other functions do, see [the corresponding
documentation](https://dotty.epfl.ch/docs/reference/dropped-features/limit22.html).

## Examples

- [depfuntype.scala](https://github.com/lampepfl/dotty/blob/master/tests/pos/depfuntype.scala)

- [eff-dependent.scala](https://github.com/lampepfl/dotty/blob/master/tests/run/eff-dependent.scala)

### Type Checking

After desugaring no additional typing rules are required for dependent function types.
