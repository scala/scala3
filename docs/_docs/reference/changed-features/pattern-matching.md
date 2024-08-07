---
layout: doc-page
title: "Option-less pattern matching"
nightlyOf: https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html
---

The implementation of pattern matching in Scala 3 was greatly simplified compared to Scala 2. From a user perspective, this means that Scala 3 generated patterns are a _lot_ easier to debug, as variables all show up in debug modes and positions are correctly preserved.

Scala 3 supports a superset of Scala 2 [extractors](https://www.scala-lang.org/files/archive/spec/2.13/08-pattern-matching.html#extractor-patterns).

## Extractors

Extractors are objects that expose a method `unapply` or `unapplySeq`:

```scala
def unapply(x: T): U
def unapplySeq(x: T): U
```

Where `T` is an arbitrary type, if it is a subtype of the scrutinee's type `Scrut`, a [type test](../other-new-features/type-test.md) is performed before calling the method.
`U` follows rules described in [Fixed Arity Extractors](#fixed-arity-extractors-1) and [Variadic Extractors](#variadic-extractors-1).

**Note:** `U` can be the type of the extractor object.

`unapply` and `unapplySeq` can actually have a more general signature, allowing for a leading type clause, as well as arbitrarily many using clauses, both before and after the regular term clause, and at most one implicit clause at the end, for example:

```scala
def unapply[A, B](using C)(using D)(x: T)(using E)(using F)(implicit y: G): U = ???
```

Extractors that expose the method `unapply` are called fixed-arity extractors, which
work with patterns of fixed arity. Extractors that expose the method `unapplySeq` are
called variadic extractors, which enables variadic patterns.

## Fixed-Arity Extractors

Fixed-arity extractors expose the following signature (with potential type, using and implicit clauses):


```scala
def unapply(x: T): U
```

The type `U` conforms to one of the following matches:

- [Boolean match](#boolean-match-1)
- [Product match](#product-match-1)

Or `U` conforms to the type `R`:

```scala
type R = {
  def isEmpty: Boolean
  def get: S
}
```

and `S` conforms to one of the following matches:

- [single match](#single-match-1)
- [name-based match](#name-based-match-1)

The former form of `unapply` has higher precedence, and _single match_ has higher
precedence over _name-based match_.

**Note:** the `S` in `R` can be `U`.

A usage of a fixed-arity extractor is irrefutable if one of the following condition holds:

- `U = true`
- the extractor is used as a product match
- `U <: R` and `U <: { def isEmpty: false }`
- `U = Some[T]`

**Note:** The last rule is necessary because, for compatibility reasons, `isEmpty` on `Some` has return type `Boolean` rather than `false`, even though it always returns `false`.

### Boolean Match

- `U =:= Boolean`
- Pattern-matching on exactly `0` patterns

For example:

<!-- To be kept in sync with tests/new/patmat-spec.scala -->

```scala
object Even:
  def unapply(s: String): Boolean = s.size % 2 == 0

"even" match
  case s @ Even() => println(s"$s has an even number of characters")
  case s          => println(s"$s has an odd number of characters")

// even has an even number of characters
```

### Product Match

- `U <: Product`
- `N > 0` is the maximum number of consecutive (`val` or parameterless `def`) `_1: P1` ... `_N: PN` members in `U`
- Pattern-matching on exactly `N` patterns with types `P1, P2, ..., PN`

For example:

<!-- To be kept in sync with tests/new/patmat-spec.scala -->

```scala
class FirstChars(s: String) extends Product:
  def _1 = s.charAt(0)
  def _2 = s.charAt(1)

   // Not used by pattern matching: Product is only used as a marker trait.
  def canEqual(that: Any): Boolean = ???
  def productArity: Int = ???
  def productElement(n: Int): Any = ???

object FirstChars:
  def unapply(s: String): FirstChars = new FirstChars(s)

"Hi!" match
  case FirstChars(char1, char2) =>
    println(s"First: $char1; Second: $char2")

// First: H; Second: i
```

### Single Match

- Pattern-matching on `1` pattern with type `S`

For example, where `Nat <: R`, `S = Int`:

<!-- To be kept in sync with tests/new/patmat-spec.scala -->

```scala
class Nat(val x: Int):
  def get: Int = x
  def isEmpty = x < 0

object Nat:
  def unapply(x: Int): Nat = new Nat(x)

5 match
  case Nat(n) => println(s"$n is a natural number")
  case _      => ()

// 5 is a natural number
```

### Name-based Match

- `S` has `N > 1` members such that they are each `val`s or parameterless `def`s, and named from `_1` with type `P1` to `_N` with type `PN`
- `S` doesn't have `N+1` members satisfying the previous point, i.e. `N` is maximal
- Pattern-matching on exactly `N` patterns with types `P1, P2, ..., PN`

For example, where `U = AlwaysEmpty.type <: R`, `S = NameBased`:
```scala
object MyPatternMatcher:
  def unapply(s: String) = AlwaysEmpty

object AlwaysEmpty:
  def isEmpty = true
  def get = NameBased

object NameBased:
  def _1: Int = ???
  def _2: String = ???

"" match
  case MyPatternMatcher(_, _) => ???
  case _ => ()
```

## Variadic Extractors

Variadic extractors expose the following signature (with potential type, using and implicit clauses):

```scala
def unapplySeq(x: T): U
```

Where `U` has to fullfill the following:

1. Set `V := U`
2. `V` is valid if `V` conforms to one of the following matches:
- [sequence match](#sequence-match-1)
- [product-sequence match](#product-sequence-match-1)
3. Otherwise `U` has to conform to the type `R`:
```scala
type R = {
  def isEmpty: Boolean
  def get: S
}
```
4. Set `V := S`, and reattempt 2., if it fails `U` is not valid.

The `V := U` form of `unapplySeq` has higher priority, and _sequence match_ has higher
precedence over _product-sequence match_.

**Note:** This means `isEmpty` is disregarded if the `V := U` form is valid

A usage of a variadic extractor is irrefutable if one of the following conditions holds:

- the extractor is used directly as a sequence match or product-sequence match
- `U <: R` and `U <: { def isEmpty: false }`
- `U = Some[T]`

**Note:** The last rule is necessary because, for compatibility reasons, `isEmpty` on `Some` has return type `Boolean` rather than `false`, even though it always returns `false`.

**Note:** Be careful, by the first condition and the note above, it is possible to define an irrefutable extractor with a `def isEmpty: true`.
This is strongly discouraged and, if found in the wild, is almost certainly a bug.

### Sequence Match

- `V <: X`

```scala
type X = {
  def lengthCompare(len: Int): Int // or, `def length: Int`
  def apply(i: Int): T1
  def drop(n: Int): scala.Seq[T2]
  def toSeq: scala.Seq[T3]
}
```
- `T2` and `T3` conform to `T1`
- Pattern-matching on _exactly_ `N` simple patterns with types `T1, T1, ..., T1`, where `N` is the runtime size of the sequence, or
- Pattern-matching on `>= N` simple patterns and _a vararg pattern_ (e.g., `xs: _*`) with types `T1, T1, ..., T1, Seq[T1]`, where `N` is the minimum size of the sequence.

For example, where `V = S`, `U = Option[S] <: R`, `S = Seq[Char]`

<!-- To be kept in sync with tests/new/patmat-spec.scala -->

```scala
object CharList:
  def unapplySeq(s: String): Option[Seq[Char]] = Some(s.toList)

"example" match
  case CharList(c1, c2, c3, c4, _, _, _) =>
    println(s"$c1,$c2,$c3,$c4")
  case _ =>
    println("Expected *exactly* 7 characters!")

// e,x,a,m
```

### Product-Sequence Match

- `V <: Product`
- `N > 0` is the maximum number of consecutive (`val` or parameterless `def`) `_1: P1` ... `_N: PN` members in `V`
- `PN` conforms to the signature `X` defined in Seq Pattern
- Pattern-matching on exactly `>= N` patterns, the first `N - 1` patterns have types `P1, P2, ... P(N-1)`,
  the type of the remaining patterns are determined as in Seq Pattern.

For example, where `V = S`, `U = Option[S] <: R`, `S = (String, PN) <: Product`, `PN = Seq[Int]`

```scala
class Foo(val name: String, val children: Int*)
object Foo:
  def unapplySeq(f: Foo): Option[(String, Seq[Int])] =
    Some((f.name, f.children))

def foo(f: Foo) = f match
  case Foo(name, x, y, ns*) => ">= two children."
  case Foo(name, ns*)       => "< two children."
```

There are plans for further simplification, in particular to factor out _product match_
and _name-based match_ into a single type of extractor.

## Type testing

Abstract type testing with `ClassTag` is replaced with `TypeTest` or the alias `Typeable`.

- pattern `_: X` for an abstract type requires a `TypeTest` in scope
- pattern `x @ X()` for an unapply that takes an abstract type requires a `TypeTest` in scope

[More details on `TypeTest`](../other-new-features/type-test.md)
