---
layout: blog-page
title: Announcing Dotty 0.7.0 and 0.8.0-RC1
author: Allan Renucci
authorImg: images/allan.jpg
date: 2018-04-27
---

Today, we are excited to release Dotty versions 0.7.0 and 0.8.0-RC1. These releases
serve as a technology preview that demonstrates new language features and the compiler supporting them.

Dotty is the project name for technologies that are considered for inclusion in Scala 3. Scala has
pioneered the fusion of object-oriented and functional programming in a typed setting. Scala 3 will
be a big step towards realizing the full potential of these ideas. Its main objectives are to

- become more opinionated by promoting programming idioms we found to work well,
- simplify where possible,
- eliminate inconsistencies and surprising behaviors,
- build on strong foundations to ensure the design hangs well together,
- consolidate language constructs to improve the language’s consistency, safety, ergonomics, and performance.

You can learn more about Dotty on our [website](https://dotty.epfl.ch).

<!--more-->

This is our eighth scheduled release according to our [6-week release schedule](https://dotty.epfl.ch/docs/usage/version-numbers.html).
The [previous technology preview](https://github.com/lampepfl/dotty/releases/tag/0.7.0-RC1) simplified
enums, introduced erased terms, improved IDE support and improved pattern matching for GADT.

## What’s new in the 0.8.0-RC1 technology preview?

### sbt 1 support [#3872](https://github.com/lampepfl/dotty/pull/3872)
Starting with Dotty 0.8.0, we will only support versions of sbt >= 1.1.4. Migrating to sbt 1
lets us use the new improved incremental compiler for Scala called [Zinc](https://github.com/sbt/zinc),
and enables integration with tools such as [Bloop](https://scalacenter.github.io/bloop/).

If you are already using Dotty with sbt 0.13, follow these simple steps to upgrade:

- update sbt version to 1.1.4 in `project/build.properties`
- update sbt-dotty plugin to the latest version:
  ```scala
  addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.2.2")
  ```
- replace usages of `.withDottyCompat()` by `.withDottyCompat(scalaVersion.value)`

### Unchecked warnings [#4045](https://github.com/lampepfl/dotty/pull/4045)
Dotty now emits `unchecked` warnings like `scalac` whenever a type test is performed but cannot be
fully checked at runtime because of type erasure. For example:

```scala
scala> def foo(x: Any) = x.isInstanceOf[List[String]]
1 |def foo(x: Any) = x.isInstanceOf[List[String]]
  |                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  |               the type test for List[String] cannot be checked at runtime
```

In some cases, the Dotty compiler is smarter than `scalac`  and will not emit a warning:
```scala
trait Marker

def foo[T](x: T) = x match {
  case _: T with Marker  => // scalac emits a spurious warning
  case _ =>
}
```

### Kind Polymorphism [#4108](https://github.com/lampepfl/dotty/pull/4108)
Normally type parameters in Scala are partitioned into kinds. First-level types are types of values.
Higher-kinded types are type constructors such as `List` or `Map`. The kind of a type is indicated
by the top type of which it is a subtype. Normal types are subtypes of `Any`, covariant single
argument type constructors such as List are subtypes of `[+X] =>> Any`, and the `Map` type
constructor is a subtype of `[X, +Y] =>> Any`.

Sometimes we would like to have type parameters that can have more than one kind, for instance to
define an implicit value that works for parameters of any kind. This is now possible through a form
of (subtype) kind polymorphism. Kind polymorphism relies on the special type `scala.AnyKind` that
can be used as an upper bound of a type.

```scala
def f[T <: AnyKind] = ..
```

The actual type arguments of f can then be types of arbitrary kinds. So the following would all be
legal:

```scala
f[Int]
f[List]
f[Map]
f[[X] =>> String]
```

**Note**: This feature is considered experimental and is only enabled under a compiler flag
(i.e. `-Ykind-polymorphism`). For more information, visit the [Kind Polymorphism](https://dotty.epfl.ch/docs/reference/other-new-features/kind-polymorphism.html)
section of our documentation.

### Improved support for SAM type [#4152](https://github.com/lampepfl/dotty/pull/4152)
This release includes fixes to [SAM types](https://www.scala-lang.org/news/2.12.0/#lambda-syntax-for-sam-types)
that greatly improve interoperability with Java 8 lambdas. One can now easily write Scala code that
uses Java streams:

```scala
val myList =
  java.util.Arrays.asList("a1", "a2", "b1", "c2", "c1")

myList
  .stream
  .filter(s => s.startsWith("c"))
  .map(_.toUpperCase)
  .sorted
  .forEach(println(_))

// prints:
// C1
// C2
```

## Trying out Dotty
### Scastie
[Scastie], the online Scala playground, supports Dotty.
This is an easy way to try Dotty without installing anything.

### sbt
Using sbt 1.1.4 or newer, do:

```shell
sbt new lampepfl/dotty.g8
```

This will setup a new sbt project with Dotty as compiler. For more details on
using Dotty with sbt, see the
[example project](https://github.com/lampepfl/dotty-example-project).

### IDE support
It is very easy to start using the Dotty IDE in any Dotty project by following
the IDE sections of the [getting-started page](https://docs.scala-lang.org/scala3/getting-started.html).


### Standalone installation
Releases are available for download on the _Releases_
section of the Dotty repository:
[https://github.com/lampepfl/dotty/releases](https://github.com/lampepfl/dotty/releases)

We also provide a [homebrew](https://brew.sh/) package that can be installed by running:

```shell
brew install lampepfl/brew/dotty
```

In case you have already installed Dotty via brew, you should instead update it:

```shell
brew upgrade dotty
```

## Let us know what you think!
If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing
Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.7.0..0.8.0-RC1` these are:

```
    95  Martin Odersky
    91  liu fengyun
    91  Nicolas Stucki
    84  Allan Renucci
    73  Guillaume Martres
    67  Martin Duhem
    18  Jendrik Wenke
    16  Paolo G. Giarrusso
     8  Robert Stoll
     6  Thierry Treyer
     4  Aggelos Biboudis
     1  tokkiyaa
     1  Rajesh Veeranki
     1  Maxime Kjaer
     1  Saurabh Rawat
     1  Joan
     1  Jorge Vicente Cantero
     1  Jasper Moeys
     1  Piotr Gabara
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://dotty.epfl.ch/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/lampepfl/dotty/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
They make perfect entry-points into hacking on the compiler.

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
