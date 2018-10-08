---
layout: doc-page
title: "Type Lambdas - More Details"
---

## Syntax

```
Type              ::=  ... |  HkTypeParamClause ‘=>’ Type
HkTypeParamClause ::=  ‘[’ HkTypeParam {‘,’ HkTypeParam} ‘]’
HkTypeParam       ::=  {Annotation} [‘+’ | ‘-’] (Id[HkTypeParamClause] | ‘_’) TypeBounds
TypeBounds        ::=  [‘>:’ Type] [‘<:’ Type]
```

### Type Checking

A type lambda such `[X] => F[X]` defines a function from types to types. The parameter(s) may carry bounds and variance annotations.
If a parameter is is bounded, as in `[X >: L <: H] => F[X]` it is checked that arguments to the parameters conform to the bounds `L` and `H`.
Only the upper bound `H` can be F-bounded, i.e. `X` can appear in it.

A variance annotation on a parameter indicates a subtyping relationship on type instances. For instance, given
```
type TL1 = [+A] => F[A]
type TL2 = [-A] => F[A]
```
and two types `S <: T`, we have
```
TL1[S] <: TL1[T
TL2[T] <: TL2[S]
```
It is checked that variance annotations on parameters of type lambdas are respected by the parameter occurrences on the
type lambda's body.

## Subtyping Rules

Assume two type lambdas
```
type TL1  =  [v1 X >: L1 <: U1] => R1
type TL2  =  [v2 X >: L2 <: U2] => R2
```
where `v1` and `v2` are optional variance annotations: `+`, `-`, or absent.
Then `TL1 <: TL2`, if the type interval `L2..U2` is contained in the type interval `L1..U1` (i.e.
`L1 <: L2` and `U2 <: U1`), and either `v2` is absent or `v1 = v2`.

## Relationship with Parameterized Type Definitions

A parameterized type definition
```
type T[X] = R
```
is regarded as a shorthand for an unparameterized definition with a type lambda as right-hand side:
```
type T = [X] => R
```

A parameterized abstract type
```
type T[X] >: L <: U
```
is regarded as shorthand for an unparameterized abstract type with type lambdas as bounds.
```
type T >: ([X] => L) <: ([X] => U)
```
However, if `L` is `Nothing` it is not parameterized, since `Nothing` is treated as a bottom type for all kinds. For instance,
```
type T[-X] <: X => ()
```
is expanded to
```
type T >: Nothing <: ([-X] => X => ())
```
instead of
```
type T >: ([X] => Nothing) <: ([-X] => X => ())
```

**Note**: The decision to treat `Nothing` as universal bottom type is provisional, and might be changed afer further discussion.

**Note**: Scala 2 and 3 differ in that Scala 2 also treats `Any` as universal top-type. This is not done in Scala 3. See also the discussion on [kind polymorphism](./kind-polymorphism.html)






