---
layout: doc-page
title: "Opaque Type Aliases: More Details"
---

### Syntax

```
Modifier          ::=  ...
                    |  ‘opaque’
```
`opaque` is a [soft modifier](../soft-modifier.html). It can still be used as a normal identifier when it is not in front of a definition keyword.

Opaque type aliases must be members of classes, traits, or objects, or they are defined
at the top-level. They cannot be defined in local blocks.

### Type Checking

The general form of a (monomorphic) opaque type alias is
```scala
opaque type T >: L <: U = R
```
where the lower bound `L` and the upper bound `U` may be missing, in which case they are assumed to be `scala.Nothing` and `scala.Any`, respectively. If bounds are given, it is checked that the right hand side `R` conforms to them, i.e. `L <: R` and `R <: U`.

Inside the scope of the alias definition, the alias is transparent: `T` is treated
as a normal alias of `R`. Outside its scope, the alias is treated as the abstract type
```scala
type T >: L <: U`
```
A special case arises if the opaque type is defined in an object. Example:
```
object o {
  opaque type T = R
}
```
In this case we have inside the object (also for non-opaque types) that `o.T` is equal to
`T` or its expanded form `o.this.T`. Equality is understood here as mutual subtyping, i.e.
`o.T <: o.this.T` and `o.this.T <: T`. Furthermore, we have by the rules of opaque types
that `o.this.T` equals `R`. The two equalities compose. That is, inside `o`, it is
also known that `o.T` is equal to `R`. This means the following code type-checks:
```scala
object o {
  opaque type T = Int
  val x: Int = id(2)
}
def id(x: o.T): o.T = x
```

### Relationship to SIP 35

Opaque types in Dotty are an evolution from what is described in
[Scala SIP 35](https://docs.scala-lang.org/sips/opaque-types.html).

The differences compared to the state described in this SIP are:

 1. Opaque type aliases cannot be defined anymore in local statement sequences.
 2. The scope where an opaque type alias is visible is now the whole scope where
    it is defined, instead of just a companion object.
 3. The notion of a companion object for opaque type aliases has been dropped.
 4. Opaque type aliases can have bounds.
 5. The notion of type equality involving opaque type aliases has been clarified. It was
    strengthened with respect to the previous implementation of SIP 35.

