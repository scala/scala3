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
Here `=:=` is understood to mean that left and right hand sides are mutatually subtypes of each other.

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
    case t <: AnyVal => t
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
if `Ci_1, ..., Ci_k` is a maximal non-empty subset of `C1, ..., Cn` such that for each `i_j`:
```
    Match(S, C1, ..., Cn)  can-reduce  i_j, Ti_j
```
and
```
    T = Ti_1 & ... & Ti_k
```
In other words, a match reduces to the intersection of all right hand sides it can reduce to. This "parallel" notion of reduction was picked for its nice algebraic properties, even though it does not correspond directly to the operational semantics of pattern matching on terms, where the first matching case is chosen.

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

Typing rules for match expressions have to account for the difference between sequential match on the term level and parallel match on the type level. As a running example consider:
```scala
  type M[X] = X match {
    case A => 1
    case B => 2
  }
  def m[X](x: X): M[X] = x match {
    case _: A => 1
    case _: B => 2
  }
```
As a first approximation, the typing rules for match expressions are as usual. E.g. to typecheck the first case `case _: A => 1` of the definition of `m` above, GADT matching will produce the constraint `X <: A`. Therefore, `M[X]` reduces to the singleton type `1`.
The right hand side `1` of the case conforms to this type, so the case typechecks. Typechecking the second case proceeds similarly.

However, it turns out that these rules are not enough for type soundness. To see this, assume that `A` and `B` are traits that are both extended by a common class `C`. In this case, `M[C]` reduces to `1 & 2`, but `m(new C)` reduces to `1`. So the type of the application `m(new C)` does not match the reduced result type of `m`, which means soundness is violated.

To plug the soundness hole, we have to tighten the typing rules for match expressions. In the example above we need to also consider the case where the scrutinee type `X` is a subtype of `A` and `B`. In this case, the match expression still returns `1` but the match type `M[X]` reduces to `1 & 2`, which means there should be a type error. However, this second check can be omitted if `A` and `B` are types that don't overlap. We can omit the check because in that case there is no scrutinee value `x` that could reduce to `1`, so no discrepancy can arise at runtime.

More generally, we proceeed as follows:

When typechecking the `i`th case of a match expression
```
  t match { case P_1 => t_1 ... case P_n => t_n
```
where `t` has type `T` and `t_i` has type `T_i`
against an expected match type `R`:

 1. Determine all maximal sequences of
    patterns `P_j_1, ..., P_j_m` that follow `P_i` in the match expression and that do overlap with `P_i`. That is, `P_i, P_j_1, ..., P_j_m` all match at least one common value.

 2. For each such sequence, verify that `T_i <: R` under the GADT constraint arising from      matching the scrutinee type `T` against all of the patterns `P_i, P_j_1, ..., P_j_m`.

In the example above, `A` and `B` would be overlapping because they have the common subclass `C`. Hence, we have to check that the right-hand side `1` is a subtype of `M[X]`
under the assumptions that `X <: A` and `X <: B`. Under these assumptions `M[X]` reduces
to `1 & 2`, which gives a type error.

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



