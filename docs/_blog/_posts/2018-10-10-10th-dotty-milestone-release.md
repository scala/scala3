---
layout: blog-page
title: Announcing Dotty 0.10.0-RC1
author: Allan Renucci
authorImg: images/allan.jpg
date: 2018-10-10
---

After a long summer break, we are excited to release Dotty version 0.10.0-RC1.
This release serves as a technology preview that demonstrates new language features and the
compiler supporting them.

Dotty is the project name for technologies that are considered for inclusion in Scala 3. Scala has
pioneered the fusion of object-oriented and functional programming in a typed setting. Scala 3 will
be a big step towards realising the full potential of these ideas. Its main objectives are to

- become more opinionated by promoting programming idioms we found to work well,
- simplify where possible,
- eliminate inconsistencies and surprising behaviours,
- build on strong foundations to ensure the design hangs well together,
- consolidate language constructs to improve the language’s consistency, safety, ergonomics, and
  performance.

You can learn more about Dotty on our [website](https://dotty.epfl.ch).

<!--more-->

This is our 10th scheduled release according to our
[6-week release schedule](https://dotty.epfl.ch/docs/usage/version-numbers.html).

## What’s new in the 0.10.0-RC1 technology preview?

### Java 9+

Dotty now supports the latest versions of Java including Java 11!

### Type-level programming: Match Types

We've introduced a new form of types called match types. A match types is a mechanism for checking a
type against a pattern. A match type reduces to one of a number of right hand sides, depending on a
scrutinee type. E.g:

```scala
type Elem[X] = X match {
  case String => Char
  case Array[t] => t
  case Iterable[t] => t
}
```

This defines a type that, depending on the scrutinee type `X`, can reduce to one of its right hand
sides. For instance,

```scala
Elem[String]       =:=  Char
Elem[Array[Int]]   =:=  Int
Elem[List[Float]]  =:=  Float
Elem[Nil]          =:=  Nothing
```

Here `=:=` is understood to mean that left and right hand sides are mutually subtypes of each other.

This feature is still experimental and subject to changes. For more information, visit the
[Match Types](https://dotty.epfl.ch/docs/reference/new-types/match-types.html) section of our documentation.

### Documentation in the REPL

The previous release added documentation support for the IDE. Users can now query the documentation
of sources previously compiled with Dotty within the REPL:

```scala
scala> /** An object */ object O { /** A def */ def foo = 0 }
// defined object O

scala> :doc O
/** An object */

scala> :doc O.foo
/** A def */
```

### Tail-recursive methods can now be polymorphic

Previously, a tail recursive call would be optimised only if the type arguments of the method
or the enclosing class did not change at call site. E.g.

```scala
@tailrec def loop[T](x: T): Int = {
  ...
  loop[Int](1)
}
```

```shell
   loop[Int](1)
   ^^^^^^^^^^^^
  Cannot rewrite recursive call: it changes type arguments on a polymorphic recursive call
```

This restriction has now been removed. We also  improve upon `scalac` which is not able to optimise
methods that change the type of `this` on a polymorphic recursive call.
[Examples](https://github.com/lampepfl/dotty/blob/7a45a4a386d33180e5b7b21aa74271a77cce4707/tests/neg-tailcall/tailrec.scala#L43-L44)
can be found in our test suite.

### Experimental support for generic Tuples

We augmented the `Tuple` class with generic methods such as `head`, `tail`, `apply`, `*:` and `++`.

```scala
scala> val t0 = (1, "2", 3L)
val t0: (Int, String, Long) = (1,2,3)

scala> val head = t0.head
val head: Int = 1

scala> val tail = t0.tail
val tail: (String, Long) = (2,3)

scala> val t1 = 0.0 *: t0
val t1: (Double, Int, String, Long) = (0.0,1,2,3)

scala> val t2 = t0 ++ t0
val t2: (Int, String, Long, Int, String, Long) = (1,2,3,1,2,3)
```

### And much more!

Please read our [release notes](https://github.com/lampepfl/dotty/releases/tag/0.10.0-RC1)
for more details!

## Breaking changes

Dotty 0.10.0-RC1 requires sbt-dotty 0.2.4 and sbt 1.2.3 or newer.

## Trying out Dotty

### sbt

You can setup a new sbt project with Dotty as the compiler by running:

```shell
sbt new lampepfl/dotty.g8
```

For more details on using Dotty with sbt, see the
[example project](https://github.com/lampepfl/dotty-example-project).

### [Mill](http://www.lihaoyi.com/mill/)

The Mill build tool version 0.2.6 introduced experimental support for Dotty. For more details on
using Dotty with Mill, see the
[example project](https://github.com/lampepfl/dotty-example-project/tree/mill).

### IDE support

Start using the Dotty IDE in any Dotty project by following
the IDE sections of the [getting-started page](https://docs.scala-lang.org/scala3/getting-started.html).

### Standalone installation

Releases are available for download on the _Releases_
section of the Dotty repository:
[https://github.com/lampepfl/dotty/releases](https://github.com/lampepfl/dotty/releases)

For macOS users, we also provide a [homebrew](https://brew.sh/) package that can be installed by
running:

```shell
brew install lampepfl/brew/dotty
```

In case you have already installed Dotty via `brew`, you should instead update it:

```shell
brew upgrade dotty
```

## Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.9.0..0.10.0-RC1` these are:

```
   219  Martin Odersky
   142  Nicolas Stucki
    67  Paolo G. Giarrusso
    52  Allan Renucci
    48  Guillaume Martres
    39  Martin Duhem
    23  Liu Fengyun
    15  Olivier Blanvillain
    10  Dmytro Melnychenko
    10  Abel Nieto
    10  Sébastien Doeraene
     7  Jaemin Hong
     7  Eugene Melekhov
     5  Saloni Vithalani
     3  Daniel Li
     3  Dale Wijnand
     3  Jasper Moeys
     2  lloydmeta
     2  Aggelos Biboudis
     2  Greg Pevnev
     1  Adriaan Moors
     1  Lukas Rytz
     1  Kazuhiro Sera
     1  Justin du Coeur, AKA Mark Waks
     1  Jan Rock
     1  Fengyun Liu
     1  Szymon Pajzert
     1  Chris Birchall
     1  benkobalog
     1  Martijn Hoekstra
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://dotty.epfl.ch/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/lampepfl/dotty/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
They make perfect entry points into hacking on the compiler.

We are looking forward to having you join the team of contributors.

## Library authors: Join our community build

Dotty now has a set of widely-used community libraries that are built against every nightly Dotty
snapshot. Currently this includes ScalaPB, algebra, scalatest, scopt and squants.
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
