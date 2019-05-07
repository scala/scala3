---
layout: doc-page
title: "Match Types"
---

A match type reduces to one of a number of right hand sides, depending on a scrutinee type. Example:

```scala
type Elem[X] = X match {
  case String => Char
  case Array[t] => t
  case Iterable[t] => t
}
```
This defines a type that, depending on the scrutinee type `X`, can reduce to one of its right hand sides. For instance,
```scala
   Elem[String]       =:=  Char
   Elem[Array[Int]]   =:=  Int
   Elem[List[Float]]  =:=  Float
   Elem[Nil]          =:=  Nothing
```
Here `=:=` is understood to mean that left and right hand sides are mutually subtypes of each other.

In general, a match type is of the form
```scala
   S match { P1 => Tn ... Pn => Tn }
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
  type Concat[+Xs <: Tuple, +Ys <: Tuple] <: Tuple = Xs match {
    case Unit => Ys
    case x *: xs => x *: Concat[xs, Ys]
  }
```
In this definition, every instance of `Concat[A, B]`, whether reducible or not, is known to be a subtype of `Tuple`. This is necessary to make the recursive invocation `x *: Concat[xs, Ys]` type check, since `*:` demands a `Tuple` as its right operand.

## Representation of Match Types

The internal representation of a match type
```
  S match { P1 => Tn ... Pn => Tn }
```
is `Match(S, C1, ..., Cn) <: B` where each case `Ci` is of the form
```
  [Xs] => P => T
```
Here, `[Xs]` is a type parameter clause of the variables bound in pattern `Pi`. If there are no bound type variables in a case, the type parameter clause is omitted and only the function type `P => T` is kept. So each case is either a unary function type or a type lambda over a unary function type.

`B` is the declared upper bound of the match type, or `Any` if no such bound is given.
We will leave it out in places where it does not matter for the discussion. Scrutinee, bound and pattern types must be first-order types.

## Match type reduction

We define match type reduction in terms of an auxiliary relation, `can-reduce`:

```
  Match(S, C1, ..., Cn)  can-reduce  i, T'
```
if `Ci = [Xs] => P => T` and there are minimal instantiations `Is` of the type variables `Xs` such that
```
  S <: [Xs := Is] P
  T' = [Xs := Is] T
```
An instantiation `Is` is _minimal_ for `Xs` if all type variables in `Xs` that appear
covariantly and nonvariantly in `Is` are as small as possible and all type variables in `Xs` that appear contravariantly in `Is` are as large as possible. Here, "small" and "large" are understood wrt `<:`.

For simplicity, we have omitted constraint handling so far. The full formulation of subtyping tests describes them as a function from a constraint and a pair of types to
either _success_ and a new constraint or _failure_. In the context of reduction, the subtyping test `S <: [Xs := Is] P` is understood to leave the bounds of all variables
in the input constraint unchanged, i.e. existing variables in the constraint cannot be instantiated by matching the scrutinee against the patterns.

Using `can-reduce`, we can now define match type reduction proper in the `reduces-to` relation:
```
  Match(S, C1, ..., Cn)  reduces-to  T
```
if
```
  Match(S, C1, ..., Cn)  can-reduce  i, T
```
and, for `j` in `1..i-1`: `C_j` is disjoint from `C_i`, or else `S` cannot possibly match `C_j`.
See the section on overlapping patterns for an elaboration of "disjoint" and "cannot possibly match".

## Subtyping Rules for Match Types

The following rules apply to match types. For simplicity, we omit environments and constraints.

The first rule is a structural comparison between two match types:
```
  Match(S, C1, ..., Cn) <: Match(T, D1, ..., Dm)
```
`    `if
```
  S <: T,  m <= n,  Ci <: Di for i in 1..n
```
I.e. scrutinees and corresponding cases must be subtypes, no case re-ordering is allowed, but the subtype can have more cases than the supertype.

The second rule states that a match type and its redux are mutual subtypes
```
  Match(S, Cs) <: T
  T <: Match(S, Cs)
```
`     `if
```
  Match(S, Cs)  reduces-to  T
```

The third rule states that a match type conforms to its upper bound
```
  (Match(S, Cs) <: B)  <:  B
```

## Variance Laws for Match Types

Within a match type `Match(S, Cs) <: B`, all occurrences of type variables count as covariant. By the nature of the cases `Ci` this means that occurrences in pattern position are contravarant (since patterns are represented as function type arguments).

## Typing Rules for Match Expressions

Typing rules for match expressions are tricky. First, they need some new form of GADT matching for value parameters.
Second, they have to account for the difference between sequential match on the term level and parallel match on the type level. As a running example consider:
```scala
  type M[+X] = X match {
    case A => 1
    case B => 2
  }
```
We'd like to be able to typecheck
```scala
  def m[X](x: X): M[X] = x match {
    case _: A => 1 // type error
    case _: B => 2 // type error
  }
```
Unfortunately, this goes nowhere. Let's try the first case. We have: `x.type <: A` and `x.type <: X`. This tells
us nothing useful about `X`, so we cannot reduce `M` in order to show that the right hand side of the case is valid.

The following variant is more promising:
```scala
  def m(x: Any): M[x.type] = x match {
    case _: A => 1
    case _: B => 2
  }
```
To make this work, we'd need a new form of GADT checking: If the scrutinee is a term variable `s`, we can make use of
the fact that `s.type` must conform to the pattern's type and derive a GADT constraint from that. For the first case above,
this would be the constraint `x.type <: A`. The new aspect here is that we need GADT constraints over singleton types where
before we just had constraints over type parameters.

Assuming this extension, we can then try to typecheck as usual. E.g. to typecheck the first case `case _: A => 1` of the definition of `m` above, GADT matching will produce the constraint `x.type <: A`. Therefore, `M[x.type]` reduces to the singleton type `1`. The right hand side `1` of the case conforms to this type, so the case typechecks.

Typechecking the second case hits a snag, though. In general, the assumption `x.type <: B` is not enough to prove that
`M[x.type]` reduces to `2`. However we can reduce `M[x.type]` to `2` if the types `A` and `B` do not overlap.
So correspondence of match terms to match types is feasible only in the case of non-overlapping patterns.

For simplicity, we have disregarded the `null` value in this discussion. `null` does not cause a fundamental problem but complicates things somewhat because some forms of patterns do not match `null`.

## Overlapping Patterns

A complete defininition of when two patterns or types overlap still needs to be worked out. Some examples we want to cover are:

 - Two classes overlap only if one is a subtype of the other
 - A final class `C` overlaps with a trait `T` only if `C` extends `T` directly or indirectly.
 - A class overlaps with a sealed trait `T` only if it overlaps with one of the known subclasses of `T`.
 - An abstract type or type parameter `A` overlaps with a type `B` only if `A`'s upper bound overlaps with `B`.
 - A union type `A_1 | A_2` overlaps with `B` only if `A_1` overlaps with `B` or `A_2` overlaps with `B`.
 - An intersection type `A_1 & A_2` overlaps with `B` only if both `A_1` and `A_2` overlap with `B`.
 - If `C[X_1, ..., X_n]` is a case class, then the instance type `C[A_1, ..., A_n]` overlaps with the instance type `C[B_1, ..., B_n]` only if for every index `i` in `1..n`,
 if `X_i` is the type of a parameter of the class, then `A_i` overlaps with `B_i`.

 The last rule in particular is needed to detect non-overlaps for cases where the scrutinee and the patterns are tuples. I.e. `(Int, String)` does not overlap `(Int, Int)` since
`String` does not overlap `Int`.

## Handling Termination

Match type definitions can be recursive, which raises the question whether and how to check
that reduction terminates. This is currently an open question. We should investigate whether
there are workable ways to enforce that recursion is primitive.

Note that, since reduction is linked to subtyping, we already have a cycle dectection mechanism in place.
So the following will already give a reasonable error message:
```scala
  type L[X] = X match {
    case Int => L[X]
  }
  def g[X]: L[X] = ???
```

```
   |  val x: Int = g[Int]
   |               ^^^^^^
   |               found:    Test.L[Int]
   |               required: Int
```

The subtype cycle test can be circumvented by producing larger types in each recursive invocation, as in the following definitions:
```scala
  type LL[X] = X match {
    case Int => LL[LL[X]]
  }
  def gg[X]: LL[X] = ???
```
In this case subtyping enters into an infinite recursion. This is not as bad as it looks, however, because
`dotc` turns selected stack overflows into type errors. If there is a stackoverflow during subtyping,
the exception will be caught and turned into a compile-time error that indicates
a trace of the subtype tests that caused the overflow without showing a full stacktrace.
Concretely:
```
   |  val xx: Int = gg[Int]
   |                  ^
   |Recursion limit exceeded.
   |Maybe there is an illegal cyclic reference?
   |If that's not the case, you could also try to increase the stacksize using the -Xss JVM option.
   |A recurring operation is (inner to outer):
   |
   |  subtype Test.LL[Int] <:< Int
   |  subtype Test.LL[Int] <:< Int
   |  ...
   |  subtype Test.LL[Int] <:< Int
```
(The actual error message shows some additional lines in the stacktrace).

## Related Work

Match types have similarities with [closed type families](https://wiki.haskell.org/GHC/Type_families) in Haskell. Some differences are:

  - Subtyping instead of type equalities.
  - Match type reduction does not tighten the underlying constraint, whereas type family reduction does unify. This difference in approach mirrors the difference between local type inference in Scala and global type inference in Haskell.
  - No a-priori requirement that cases are non-overlapping. Uses parallel reduction
    instead of always chosing a unique branch.

Match types are also similar to Typescript's [conditional types](https://github.com/Microsoft/TypeScript/pull/21316). The main differences here are:

 - Conditional types only reduce if scrutinee and pattern are ground, whereas
   match types also work for type parameters and abstract types.
 - Match types can bind variables in type patterns.
 - Match types support direct recursion.

Conditional types in Typescript distribute through union types. We should evaluate whether match types should support this as well.


