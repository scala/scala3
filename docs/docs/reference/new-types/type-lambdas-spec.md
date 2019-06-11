---
layout: doc-page
title: "Type Lambdas - More Details"
---

## Syntax

```
Type              ::=  ... |  HkTypeParamClause ‘=>>’ Type
HkTypeParamClause ::=  ‘[’ HkTypeParam {‘,’ HkTypeParam} ‘]’
HkTypeParam       ::=  {Annotation} [‘+’ | ‘-’] (Id[HkTypeParamClause] | ‘_’) TypeBounds
TypeBounds        ::=  [‘>:’ Type] [‘<:’ Type]
```

### Type Checking

A type lambda such `[X] =>> F[X]` defines a function from types to types. The parameter(s) may carry bounds and variance annotations.
If a parameter is is bounded, as in `[X >: L <: H] =>> F[X]` it is checked that arguments to the parameters conform to the bounds `L` and `H`.
Only the upper bound `H` can be F-bounded, i.e. `X` can appear in it.

A variance annotation on a parameter indicates a subtyping relationship on type instances. For instance, given
```scala
type TL1 = [+A] =>> F[A]
type TL2 = [-A] =>> F[A]
```
and two types `S <: T`, we have
```scala
TL1[S] <: TL1[T]
TL2[T] <: TL2[S]
```
It is checked that variance annotations on parameters of type lambdas are respected by the parameter occurrences on the type lambda's body.

**Note** No requirements hold for the variances of occurrences of type variables in their bounds. It is an open question whether we need to impose additional requirements here
(`scalac` doesn't check variances in bounds either).

## Subtyping Rules

Assume two type lambdas
```scala
type TL1  =  [v1 X >: L1 <: U1] =>> R1
type TL2  =  [v2 X >: L2 <: U2] =>> R2
```
where `v1` and `v2` are optional variance annotations: `+`, `-`, or absent.
Then `TL1 <: TL2`, if

 - the type interval `L2..U2` is contained in the type interval `L1..U1` (i.e.
`L1 <: L2` and `U2 <: U1`),
 - either `v2` is absent or `v1 = v2`
 - `R1 <: R2`

Here we have relied on alpha renaming to bring match the two bound types `X`.

A partially applied type constructor such as `List` is assumed to be equivalent to
its eta expansion. I.e, `List = [+X] =>> List[X]`. This allows type constructors
to be compared with type lambdas.

## Relationship with Parameterized Type Definitions

A parameterized type definition
```scala
type T[X] = R
```
is regarded as a shorthand for an unparameterized definition with a type lambda as right-hand side:
```scala
type T = [X] =>> R
```

A parameterized abstract type
```scala
type T[X] >: L <: U
```
is regarded as shorthand for an unparameterized abstract type with type lambdas as bounds.
```scala
type T >: ([X] =>> L) <: ([X] =>> U)
```
However, if `L` is `Nothing` it is not parameterized, since `Nothing` is treated as a bottom type for all kinds. For instance,
```scala
type T[-X] <: X => ()
```
is expanded to
```scala
type T >: Nothing <: ([-X] =>> X => ())
```
instead of
```scala
type T >: ([X] =>> Nothing) <: ([-X] =>> X => ())
```

The same expansions apply to type parameters. E.g.
```scala
[F[X] <: Coll[X]]
```
is treated as a shorthand for
```scala
[F >: Nothing <: [X] =>> Coll[X]]
```

**Note**: The decision to treat `Nothing` as universal bottom type is provisional, and might be changed afer further discussion.

**Note**: Scala 2 and 3 differ in that Scala 2 also treats `Any` as universal top-type. This is not done in Scala 3. See also the discussion on [kind polymorphism](./kind-polymorphism.html)

## Curried Type Parameters

The body of a type lambda can again be a type lambda. Example:
```scala
type TL = [X] =>> [Y] =>> (X, Y)
```
Currently, no special provision is made to infer type arguments to such curried type lambdas. This is left for future work.



