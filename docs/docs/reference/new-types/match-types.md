---
layout: doc-page
title: "Match Types"
---

A match type reduces to one of its right-hand sides, depending on a scrutinee type. Example:

```scala
type Elem[X] = X match {
  case String => Char
  case Array[t] => t
  case Iterable[t] => t
}
```

This defines a type that reduces as follows:

```scala
Elem[String]       =:=  Char
Elem[Array[Int]]   =:=  Int
Elem[List[Float]]  =:=  Float
Elem[Nil.type]     =:=  Nothing
```

Here `=:=` is understood to mean that left and right hand sides are mutually subtypes of each other.

In general, a match type is of the form

```scala
S match { P1 => T1 ... Pn => Tn }
```

where `S`, `T1`, ..., `Tn` are types and `P1`, ..., `Pn` are type patterns. Type variables
in patterns start as usual with a lower case letter.

Match types can form part of recursive type definitions. Example:

```scala
type LeafElem[X] = X match {
  case String => Char
  case Array[t] => LeafElem[t]
  case Iterable[t] => LeafElem[t]
  case AnyVal => X
}
```

Recursive match type definitions can also be given an upper bound, like this:

```scala
type Concat[Xs <: Tuple, +Ys <: Tuple] <: Tuple = Xs match {
  case Unit => Ys
  case x *: xs => x *: Concat[xs, Ys]
}
```

In this definition, every instance of `Concat[A, B]`, whether reducible or not, is known to be a subtype of `Tuple`. This is necessary to make the recursive invocation `x *: Concat[xs, Ys]` type check, since `*:` demands a `Tuple` as its right operand.

## Dependent typing

Match types can be used to define dependently type methods. For instance, here is value level counterpart to the`LeafElem` defined above (note the use of the match type as return type):

```scala
def leafElem[X](x: X): LeafElem[X] = x match {
  case x: String      => x.charAt(0)
  case x: Array[t]    => leafElem(x(9))
  case x: Iterable[t] => leafElem(x.next())
  case x: AnyVal      => x
}
```

This special mode of typing for match expressions is only used when the following conditions are met:

1. The match expression patterns do not have guards
2. The match expression scrutinee's type is a subtype of the match type scrutinee's type
3. The match expression and the match type have the same number of cases
4. The match expression patterns are all [Typed Patterns](https://scala-lang.org/files/archive/spec/2.13/08-pattern-matching.html#typed-patterns), and these types are `=:=` to their corresponding type patterns in the match type

## Representation of Match Types

The internal representation of a match type
```
S match { P1 => T1 ... Pn => Tn }
```
is `Match(S, C1, ..., Cn) <: B` where each case `Ci` is of the form
```
[Xs] =>> P => T
```
Here, `[Xs]` is a type parameter clause of the variables bound in pattern `Pi`. If there are no bound type variables in a case, the type parameter clause is omitted and only the function type `P => T` is kept. So each case is either a unary function type or a type lambda over a unary function type.

`B` is the declared upper bound of the match type, or `Any` if no such bound is given.
We will leave it out in places where it does not matter for the discussion. Scrutinee, bound and pattern types must be first-order types.


## Match type reduction

Match type reduction follows the semantics of match expression, that is, a match type of the form `S match { P1 => T1 ... Pn => Tn }` reduces to `Ti` if and only if `s: S match { _: P1 => T1 ... _: Pn => Tn }` evaluates to a value of type `Ti` for all `s: S`.

The compiler implements the following reduction algorithm:

- If the scrutinee type `S` is an empty set of values (such as `Nothing` or `String & Int`), do not reduce.
- Sequentially consider each pattern `Pi`
    - If `S <: Pi` reduce to `Ti`.
    - Otherwise, try constructing a proof that `S` and `Pi` are disjoint, or, in other words, that no value `s` of type `S` is also of type `Pi`.
    - If such proof is found, proceed to the case (`Pi+1`), otherwise, do not reduce.

Disjointness proofs rely on the following properties of Scala types:

1. Single inheritance of classes
2. Final classes cannot be extended
3. Constant types with distinct values are nonintersecting


## Subtyping Rules for Match Types

The following rules apply to match types. For simplicity, we omit environments and constraints.

The first rule is a structural comparison between two match types:
```
Match(S, C1, ..., Cm) <: Match(T, D1, ..., Dn)
```
if
```
S <: T,  m >= n,  Ci <: Di for i in 1..n
```
I.e. scrutinees and corresponding cases must be subtypes, no case re-ordering is allowed, but the subtype can have more cases than the supertype.

The second rule states that a match type and its redux are mutual subtypes
```
  Match(S, Cs) <: T
  T <: Match(S, Cs)
```
if
```
  Match(S, Cs)  reduces-to  T
```

The third rule states that a match type conforms to its upper bound
```
  (Match(S, Cs) <: B)  <:  B
```

## Variance Laws for Match Types

Within a match type `Match(S, Cs) <: B`, all occurrences of type variables count as covariant. By the nature of the cases `Ci` this means that occurrences in pattern position are contravarant (since patterns are represented as function type arguments).

## Related Work

Match types have similarities with [closed type families](https://wiki.haskell.org/GHC/Type_families) in Haskell. Some differences are:

  - Subtyping instead of type equalities.
  - Match type reduction does not tighten the underlying constraint, whereas type family reduction does unify. This difference in approach mirrors the difference between local type inference in Scala and global type inference in Haskell.

Match types are also similar to Typescript's [conditional types](https://github.com/Microsoft/TypeScript/pull/21316). The main differences here are:

 - Conditional types only reduce if scrutinee and pattern are ground, whereas
   match types also work for type parameters and abstract types.
 - Match types can bind variables in type patterns.
 - Match types support direct recursion.
 - Conditional types distribute through union types.
