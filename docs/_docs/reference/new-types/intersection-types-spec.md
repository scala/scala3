---
layout: doc-page
title: "Intersection Types - More Details"
movedTo: https://docs.scala-lang.org/scala3/reference/new-types/intersection-types-spec.html
---

## Syntax

Syntactically, the type `S & T` is an infix type, where the infix operator is `&`.
The operator `&` is a normal identifier
with the usual precedence and subject to usual resolving rules.
Unless shadowed by another definition, it resolves to the type `scala.&`,
which acts as a type alias to an internal representation of intersection types.

```ebnf
Type              ::=  ...| InfixType ;
InfixType         ::=  RefinedType {id [nl] RefinedType} ;
```

## Subtyping Rules

```
T <: A    T <: B
----------------
  T <: A & B

    A <: T
----------------
    A & B <: T

    B <: T
----------------
    A & B <: T
```

From the rules above, we can show that `&` is _commutative_: `A & B <: B & A` for any type `A` and `B`.

```
   B <: B           A <: A
----------       -----------
A & B <: B       A & B <: A
---------------------------
       A & B  <:  B & A
```

In another word, `A & B` is the same type as `B & A`, in the sense that the two types
have the same values and are subtypes of each other.

If `C` is a type constructor, then `C[A] & C[B]` can be simplified using the following three rules:

- If `C` is covariant, `C[A] & C[B] ~> C[A & B]`
- If `C` is contravariant, `C[A] & C[B] ~> C[A | B]`
- If `C` is non-variant, emit a compile error

When `C` is covariant, `C[A & B] <: C[A] & C[B]` can be derived:

```
    A <: A                  B <: B
  ----------               ---------
  A & B <: A               A & B <: B
---------------         -----------------
C[A & B] <: C[A]          C[A & B] <: C[B]
------------------------------------------
      C[A & B] <: C[A] & C[B]
```

When `C` is contravariant, `C[A | B] <: C[A] & C[B]` can be derived:

```
    A <: A                        B <: B
  ----------                     ---------
  A <: A | B                     B <: A | B
-------------------           ----------------
C[A | B] <: C[A]              C[A | B] <: C[B]
--------------------------------------------------
            C[A | B] <: C[A] & C[B]
```

## Erasure

The erased type for `S & T` is the erased _glb_ (greatest lower bound) of the
erased type of `S` and `T`. The rules for erasure of intersection types are given
below in pseudocode:

```
|S & T| = glb(|S|, |T|)

glb(JArray(A), JArray(B)) = JArray(glb(A, B))
glb(JArray(T), _)         = JArray(T)
glb(_, JArray(T))         = JArray(T)
glb(A, B)                 = A                     if A extends B
glb(A, B)                 = B                     if B extends A
glb(A, _)                 = A                     if A is not a trait
glb(_, B)                 = B                     if B is not a trait
glb(A, _)                 = A                     // use first
```

In the above, `|T|` means the erased type of `T`, `JArray` refers to
the type of Java Array.

See also: [`TypeErasure#erasedGlb`](https://github.com/lampepfl/dotty/blob/main/compiler/src/dotty/tools/dotc/core/TypeErasure.scala#L289).

## Relationship with Compound Type (`with`)

Intersection types `A & B` replace compound types `A with B` in Scala 2. For the
moment, the syntax `A with B` is still allowed and interpreted as `A & B`, but
its usage as a type (as opposed to in a `new` or `extends` clause) will be
deprecated and removed in the future.
