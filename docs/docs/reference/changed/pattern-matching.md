---
layout: doc-page
title: "Option-less pattern matching"
---

Dotty implementation of pattern matching was greatly simplified compared to scalac. From a user perspective, this means that Dotty generated patterns are a *lot* easier to debug, as variables all show up in debug modes and positions are correctly preserved.

Dotty supports a superset of scalac's [extractors](https://www.scala-lang.org/files/archive/spec/2.13/08-pattern-matching.html#extractor-patterns).

## Boolean Pattern

- Extractor defines `def unapply(x: T): Boolean`
- Pattern-matching on exactly `0` patterns

For example:

<!-- To be kept in sync with tests/new/patmat-spec.scala -->

```scala
object Even {
  def unapply(s: String): Boolean = s.size % 2 == 0
}

"even" match {
  case s @ Even() => println(s"$s has an even number of characters")
  case s          => println(s"$s has an odd number of characters")
}
// even has an even number of characters
```


## Product Pattern

- Extractor defines `def unapply(x: T): U`
- `U <: Product`
- `N > 0` is the maximum number of consecutive (parameterless `def` or `val`) `_1: P1` ... `_N: PN` members in `U`
- Pattern-matching on exactly `N` patterns with types `P1, P2, ..., PN`

For example:

<!-- To be kept in sync with tests/new/patmat-spec.scala -->

```scala
class FirstChars(s: String) extends Product {
  def _1 = s.charAt(0)
  def _2 = s.charAt(1)

  // Not used by pattern matching: Product is only used as a marker trait.
  def canEqual(that: Any): Boolean = ???
  def productArity: Int = ???
  def productElement(n: Int): Any = ???
}

object FirstChars {
  def unapply(s: String): FirstChars = new FirstChars(s)
}

"Hi!" match {
  case FirstChars(char1, char2) =>
    println(s"First: $char1; Second: $char2")
}
// First: H; Second: i
```


## Name-based Seq Pattern

- Extractor defines `def unapplySeq(x: T): U`
- `U` has (parameterless `def` or `val`) members `isEmpty: Boolean` and `get: S`
- `S` conforms to `X` or `Y`,  `T2` and `T3` conform to `T1`

```Scala
type X = {
  def lengthCompare(len: Int): Int
  def apply(i: Int): T1 = a(i)
  def drop(n: Int): scala.Seq[T2]
  def toSeq: scala.Seq[T3]
}

type Y = {
  def length: Int
  def apply(i: Int): T1 = a(i)
  def drop(n: Int): scala.Seq[T2]
  def toSeq: scala.Seq[T3]
}
```

- Pattern-matching on _exactly_ `N` simple patterns with types `T1, T1, ..., T1`, where `N` is the runtime size of the sequence, or
- Pattern-matching on `>= N` simple patterns and _a vararg pattern_ (e.g., `xs: _*`) with types `T1, T1, ..., T1, Seq[T1]`, where `N` is the minimum size of the sequence.

<!-- To be kept in sync with tests/new/patmat-spec.scala -->

```scala
object CharList {
  def unapplySeq(s: String): Option[Seq[Char]] = Some(s.toList)
}

"example" match {
  case CharList(c1, c2, c3, c4, _, _, _) =>
    println(s"$c1,$c2,$c3,$c4")
  case _ =>
    println("Expected *exactly* 7 characters!")
}
// e,x,a,m
```


## Name-based Pattern

- Extractor defines `def unapply(x: T): U`
- `U` has (parameterless `def` or `val`) members `isEmpty: Boolean` and `get: S`
- If there is exactly `1` pattern, pattern-matching on `1` pattern with type `S`
- Otherwise `N > 0` is the maximum number of consecutive (parameterless `def` or `val`) `_1: P1` ... `_N: PN` members in `U`
- Pattern-matching on exactly `N` patterns with types `P1, P2, ..., PN`

<!-- To be kept in sync with tests/new/patmat-spec.scala -->

```scala
class Nat(val x: Int) {
  def get: Int = x
  def isEmpty = x < 0
}

object Nat {
  def unapply(x: Int): Nat = new Nat(x)
}

5 match {
  case Nat(n) => println(s"$n is a natural number")
  case _      => ()
}
// 5 is a natural number
```

In case of ambiguities, *Product Pattern* is preferred over *Name Based Pattern*. This document reflects the state of pattern matching as currently implemented in Dotty. There are plans for further simplification, in particular to factor out *Product Pattern* and *Name Based Pattern* into a single type of extractor.
