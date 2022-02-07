---
layout: blog-page
title: Announcing Dotty 0.21.0-RC1 - explicit nulls, new syntax for `match` and conditional givens, and more
author: Aggelos Biboudis
authorImg: images/aggelos.jpg
date: 2019-12-20
---

Greetings and we wish you Merry Christmas 🎄! We are excited to announce
0.21.0-RC1 of Dotty. In this version we add support for non-nullable reference
types, supported by flow-sensitive analysis. We present new syntax for given
extensions, matches and pattern matching over quotes. We are also happy to
announce that SemanticDB generation is now supported within the compiler, this
will eventually enable Metals to support Dotty! And last but not least ... we are
happy to announce that we are now feature complete!

# Feature Complete!

This release is a HUGE milestone for us, for Dotty, for Scala 3, for our community. Since that
[initial commit](https://github.com/lampepfl/dotty/commit/90962407e72d88f8f3249ade0f6bd60ff15af5ce)
on the 6th December of 2012 when the only feature was the basic structure of a
compiler based on the DOT calculus, we have come a long way.

7 years and 20k commits later we are happy to announce that we are now _feature-complete_ for Scala 3.
This means that with this release we stop adding new features and we focus on:

- bug fixing and general quality assurance towards the final release 🐛
- performance engineering 🏎️
- documentation improvements 📕
- education 👨‍🏫

Being feature complete does not mean that every detail of Scala 3 is cast in
stone yet. Some details can still change, or be enabled conditionally, or even
be dropped entirely. That will depend on the additional experience we gain over
the next months, in particular the feedback we receive from the community and
the SIP committee. But the envelope of what will (most likely) be in Scala 3 is
now defined and implemented.

For an overview of the feature envelope that Scala 3 carries you can read our
[Overview](https://dotty.epfl.ch/docs/reference/overview.html) page.
For a more detailed discussion on the transition to Scala 3 you can read the
[Scala 2 roadmap update: The road to Scala 3](https://www.scala-lang.org/2019/12/18/road-to-scala-3.html).

### Community build

Being feature complete doesn't mean that development slows down. On the contrary!
It means that we can now put the Scala 3 compiler under heavy load, getting it
ready for industrial strength applications. At the moment we have 23
projects on our community projects and we expect this number to go up!

> https://github.com/lampepfl/dotty/tree/main/community-build/community-projects

This project contains tests to build and test a corpus of open sources Scala 2.x
projects against Scala 3.

To run the community-build on a local machine from the main Dotty repository, first fetch all the git
submodules with `git submodule update --init` and run `sbt community-build/test`
from the root of the dotty repo.

For more details also follow the [Migrating the Ecosystem](https://www.scala-lang.org/2019/12/18/road-to-scala-3.html#migrating-the-ecosystem) on the Road to Scala 3 blogpost.

### New Issues

Firstly thank you for all the hard work in issue reporting! Being feature complete means that our
issue tracker will now be more important than ever. We encourage you to stress
the compiler and report self-contained test-cases! Bug minimization is hard and
an art form! Help us unearth those nasty bugs! ✊

Last but not least we restate the mission of Scala 3. Scala has pioneered the
fusion of object-oriented and functional programming in a typed setting and Scala 3
will be a big step towards realising the full potential of these ideas. Its main
objectives are to:

- become more opinionated by promoting programming idioms we found to work well,
- simplify where possible,
- eliminate inconsistencies and surprising behaviours,
- build on strong foundations to ensure the design hangs together well,
- consolidate language constructs to improve the language’s consistency, safety, ergonomics, and
  performance.

You can learn more about Dotty on our [website](https://dotty.epfl.ch).

<!--more-->

# What’s new in the 0.21.0-RC1 technology preview?

## Dotty with explicit nulls and flow typing

We add support for non-nullable reference types under the compiler option
`-Yexplicit-nulls`. Nullability needs then to be expressed explicitly via unions
(e.g. `String|Null`).

This means the following code will no longer typecheck:

```scala
val x: String = null // error: found `Null`,  but required `String`
```

Instead, to mark a type as nullable we use a type union:

```scala
val x: String|Null = null // ok
```

This change affects two parts of the compiler. Firstly we have a new type
hierarchy for `Null` and a _translation layer_ from Java types to Scala types,
which balances soundness and usability.

With this release we also introduce a flow-sensitive analysis that refines the
type of an expression based on control-flow. In the example below `s` is
`String|Null`. The `if` branch validates the value of `s` against `Null` so `s`
can be safely considered `String` in that scope.

```scala
val s: String|Null = ???

if (s != null) {
  // s: String
}
else {
  // s: String|Null
}
```

Note, that more complex tests are also supported like:

```scala
val s: String|Null = ???
val s2: String|Null = ???

if (s != null && s2 != null) // s: String and s2: String
```

but also in a short-circuiting manner:

```scala
val s: String|Null = ???

if (s != null && s.length > 0) // s: String in `s.length > 0`
```

To support Java Interop under explicit nulls we provide an alias for `Null`
called `UncheckedNull`. The compiler can load Java classes in two ways: from
source or from bytecode. In either case, when a Java class is loaded, we "patch"
the type of its members to reflect that Java types remain implicitly nullable.

An additional value of `UncheckedNull` (on the Scala side) is that we
effectively support method chaining on Java-returned values. e.g.,

```scala
val s2: String = someJavaMethod().trim().substring(2).toLowerCase()
```

as opposed to:

```scala
val ret = someJavaMethod()
val s2 = if (ret != null) {
  val tmp = ret.trim()
  if (tmp != null) {
    val tmp2 = tmp.substring(2)
    if (tmp2 != null) {
      tmp2.toLowerCase()
    }
  }
}
// Additionally, we need to handle the `else` branches.
```

This feature is the result of a successful collaboration between LAMP/EPFL, Abel
Nieto, Yaoyu Zhao and Ondřej Lhoták from the University of Waterloo. For more
info refer to the docs on [Explicit Nulls](https://dotty.epfl.ch/docs/reference/other-new-features/explicit-nulls.html).

## New syntax for given instances defining extension methods

To make code navigation easier in the case of `given` extension methods we
change the syntax in the following two manners. Hereafter, we write:

```scala
given listOps: extension [T](xs: List[T]) { ... }

given extension (s: String) { ... }
```
or
```scala
given listOps: [T](xs: List[T]) extended with { ... }

given (s: String) extended with { ... }
```

instead of:

```scala
given listOps: [T](xs: List[T]) { ... }

given (s: String) { ... }
```

After experimenting with both, one will be settled upon.
The rationale is to communicate in a clean way that the parameters go on the
extension method and not the wrapper (e.g., `listOps`) .

To learn more about extension methods and given instances for extension methods in particular follow the docs on [Given Instances for Extension Methods](https://dotty.epfl.ch/docs/reference/contextual/extension-methods.html#given-instances-for-extension-methods)

## New syntax for conditional givens

We are experimenting with a new way to write given instances that are conditionally provided given other instances.

Using present given syntax, it can seem awkward to define parameterised instances:
```scala
given listOrd[T](given Ord[T]): Ord[List[T]] ...
```
it's particularly unfortunate for anonymous given instances:
```scala
given [T](given Ord[T]): Ord[List[T]] ...
```
and worst in the monomorphic case:
```scala
given (given outer: Context): Context = ...
```

With the new syntax, the above definitions become
```scala
given listOrd[T]: Ord[T] => Ord[List[T]] ...

given [T]: Ord[T] => Ord[List[T]] ...

given (outer: Context) => Context = ...
```
where the `=>` is read as `implies`, e.g. "a given `Ord[T]` implies a given `Ord[List[T]]`."

This syntax is intentionally similar to function types. Indeed, multiple given parameter lists are provided as such:
```scala
given [T]: (ctx: Context) => (ctx.Type[T]) => Zero[ctx.Expr[T]]
```
where the above reads as "a given `(ctx: Context)` and a given `ctx.Type[T]` implies a given `Zero[ctx.Expr[T]]`."

As a worked example, we define a parameterised given instance for `Show[(A,B)]`:

```scala
trait Show[-A] with
  def (a: A) show: String

given Show[String] = x => x
given Show[Int]    = _.toString

given [A,B]: (Show[A], Show[B]) => Show[(A,B)] =
  (a,b) => s"(${a.show}, ${b.show})"

@main def ShowPair =
  println((1 -> "one").show) // (1, "one")
```

After a period of experimentation, either the new or old way will remain.

## New match syntax

We introduce an improved treatment of `match`. We reintroduce `match` as an
alphanumeric, left-associative, infix operator that can support chained matches:

```scala
xs match {
  case Nil => "empty"
  case x :: xs1 => "nonempty"
} match {
  case "empty" => 0
  case "nonempty" => 1
}
```

By using the new treatment we can now offer `match` as a method:

```scala
xs.match {
  case Nil => false
  case _ => true
}
```

You can read more in our docs [Match Expressions](https://dotty.epfl.ch/docs/reference/changed-features/match-syntax.html) and on the interesting discussions in [contributors](https://contributors.scala-lang.org/t/pre-sip-demote-match-keyword-to-a-method/2137/2).

## Metaprogramming: New quoted pattern matching

We introduce a high-level API to deconstruct or extract values out of `Expr`
using pattern matching. It consists of high-level extractors for getting static
information out of exprs and, of quoted patterns that allows to deconstruct
complex code that contains a precise structure, types or methods.
Patterns `'{ ... }` can be placed in any location where Scala expects a pattern.

The new extractors are summarized below:

* `scala.quoted.matching.Const`: matches an expression a literal value and returns the value.
* `scala.quoted.matching.ExprSeq`: matches an explicit sequence of expresions and returns them. These sequences are useful to get individual `Expr[T]` out of a varargs expression of type `Expr[Seq[T]]`.
* `scala.quoted.matching.ConstSeq`:  matches an explicit sequence of literal values and returns them.

The following snippet demonstrates the new _quoted patterns_ implementing a
simple, 1-level, non-recursive rewriter macro for exponents. `rewrite` is a an
inline method definition designating a macro as usual. To inspect an `expr`
value with friendly syntax we can now use the quoted syntax as patterns inside a
match expression. Notice that quotes designate patterns and `$`, the familiar
syntax for splices is used to _extract_ (capture) information out of a pattern.

```scala
inline def rewrite(expr: => Double): Double = ${rewrite('expr)}

def rewrite(expr: Expr[Double])(given QuoteContext): Expr[Double] = {
  val res = expr match {
    // product rule
    case '{ power2($a, $x) * power2($b, $y)} if a.matches(b) => '{ power2($a, $x + $y) }
    // rules of 1
    case '{ power2($a, 1)} => a
    case '{ power2(1, $a)} => '{ 1.0 }
    // rule of 0
    case '{ power2($a, 0)} => '{ 1.0 }
    // power rule
    case '{ power2(power2($a, $x), $y)} => '{ power2($a, $x * $y ) }
    case _ => expr
  }
  res
}
```

To learn more read our docs on [pattern matching over quotes](https://dotty.epfl.ch/docs/reference/metaprogramming/macros.html#pattern-matching-on-quoted-expressions).

## Added support for SemanticDB file generation

As part of ongoing efforts to support Dotty in Metals, our latest release now offers support for generation
of SemanticDB files, enabled with the `-Ysemanticdb` compiler flag.
Providing `-semanticdb-target` allows the user to select a separate target destination for the `META-INF` directory (the
root for `.semanticdb` files) and `-sourceroot` to calculate a relative path for SemanticDB files within `META-INF`.

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.20.0-RC1..0.21.0-RC1` these are:

```
   176  Martin Odersky
   171  Bryan Abate
    88  Nicolas Stucki
    81  Jamie Thompson
    44  noti0na1
    29  Anatolii
    28  bishabosha
    23  Antoine Brunner
    19  Guillaume Martres
    10  Aleksander Boruch-Gruszecki
     8  Guillaume Raffin
     6  Olafur Pall Geirsson
     5  Sébastien Doeraene
     4  Aggelos Biboudis
     4  Liu Fengyun
     4  Paolo G. Giarrusso
     3  Andrea Mocci
     3  Martijn Hoekstra
     2  Ben Elliott
     2  Patrik Mada
     2  Rafal Piotrowski
     2  odersky
     1  Markus Kahl
     1  Richard Beddington
     1  Vlastimil Dort
     1  Anatolii Kmetiuk
     1  Raphael Jolly
     1  Lucas
     1  Nikita Eshkeev
     1  Brian Wignall
     1  Olivier Blanvillain
     1  张志豪
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://dotty.epfl.ch/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/lampepfl/dotty/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
They make perfect entry points into hacking on the compiler.

We are looking forward to having you join the team of contributors.

## Library authors: Join our community build

Dotty now has a set of widely-used community libraries that are built against every nightly Dotty
snapshot. Currently, this includes ScalaPB, algebra, scalatest, scopt and squants.
Join our [community build](https://github.com/lampepfl/dotty-community-build)
to make sure that our regression suite includes your library.

[Scastie]: https://scastie.scala-lang.org/?target=dotty

[@odersky]: https://github.com/odersky
[@DarkDimius]: https://github.com/DarkDimius
[@smarter]: https://github.com/smarter
[@felixmulder]: https://github.com/felixmulder
[@nicolasstucki]: https://github.com/nicolasstucki
[@liufengyun]: https://github.com/liufengyun
[@OlivierBlanvillain]: https://github.com/OlivierBlanvillain
[@biboudis]: https://github.com/biboudis
[@allanrenucci]: https://github.com/allanrenucci
[@Blaisorblade]: https://github.com/Blaisorblade
[@Duhemm]: https://github.com/Duhemm
[@AleksanderBG]: https://github.com/AleksanderBG
[@milessabin]: https://github.com/milessabin
[@anatoliykmetyuk]: https://github.com/anatoliykmetyuk
