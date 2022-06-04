---
layout: blog-page
title: Announcing Dotty 0.11.0-RC1
author: Allan Renucci
authorImg: images/allan.jpg
date: 2018-11-30
---

Today we are excited to release Dotty version 0.11.0-RC1.
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

This is our 11th scheduled release according to our
[6-week release schedule](https://dotty.epfl.ch/docs/usage/version-numbers.html).

## What’s new in the 0.11.0-RC1 technology preview?

### Opaque Type Aliases

Opaque types aliases provide type abstraction without any overhead. Example:

```scala
opaque type Duration = Long
```

This introduces `Duration` as a new type, which is implemented as a `Long` but is different from
it. The fact that `Duration` is the same as `Long` is only known in the companion object of
`Duration`. Here is a possible companion object:

```scala
object Duration {

  // These are the ways to lift to the Duration type
  def fromNanos(duration: Long): Duration = duration
  def fromSeconds(duration: Long): Duration = duration * 1000000000

  // This is the first way to unlift the Duration type
  def toNanos(l: Duration): Long = l

  // Extension methods define opaque types' public APIs
  implicit class DurationOps(self: Duration) extends AnyVal {
    // This is the second way to unlift the Duration type
    def toSeconds: Long = self / 1000000000
    def + (that: Duration): Duration = self + that
  }
}
```

The companion object contains the `fromNanos` and `fromSeconds` methods that convert from longs to
`Duration` values. It also adds a `toNanos` function and a decorator that implements `+` on
duration values, as well as a conversion `toSeconds`. All of this is possible because within object
`Duration`, the type `Duration` is just an alias of `Long`.

Outside the companion object, `Duration` is treated as a new abstract type. So the following
operations would be valid because they use functionality implemented in the `Duration`
object.

```scala
val d1 = Duration.fromNanos(1000L)
val d2 = Duration.fromSeconds(2L)
val d3 = d1 + d2
```

But the following operations would lead to type errors:

```scala
val l: Long = d1      // error: found: Duration, required: Long
val d: Duration = 3L  // error: found: Long(3L), required: Duration
d1 + 2L               // error: found: Long(2L), required: Duration
d1 - d2               // error: `-` is not a member of Duration
```

### Worksheet Mode Support in Visual Studio Code

Dotty IDE can now be used in Worksheet mode. A worksheet is a Scala file that is evaluated on save,
and the result of each expression is displayed in a column on the right of your program. Worksheets
are like a REPL session on steroids, and enjoy 1st class editor support: completions, hyperlinking,
interactive errors-as-you-type, etc.

![]({{ site.baseurl }}/images/worksheets/worksheet-demo.gif "Run worksheet")

For more information about the worksheets, see [Worksheet mode with Dotty
IDE](https://dotty.epfl.ch/docs/usage/worksheet-mode.html)

### Various IDE improvements

#### Help with method signatures

When writing a method call, Dotty IDE will now show contextual information that helps filling in the
arguments of the method.

![]({{ site.baseurl }}/images/dotty-ide/signature-help.png "Signature help")

#### Improved display of documentation in Dotty IDE

In this release, we reworked how we show documentation inside the IDE. We now extract useful
information from the Scaladoc comment, then format it before we display it in the IDE.

![]({{ site.baseurl }}/images/dotty-ide/documentation-hover.png "Documentation hover")

### And much more!

Please read our [release notes](https://github.com/lampepfl/dotty/releases/tag/0.11.0-RC1)
for more details!

## Trying out Dotty

### sbt

You can set up a new sbt project with Dotty as the compiler by running:

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

According to `git shortlog -sn --no-merges 0.10.0..0.11.0-RC1` these are:

```
   143  Martin Duhem
   104  Nicolas Stucki
    82  Martin Odersky
    60  Guillaume Martres
    35  Allan Renucci
    21  poechsel
    12  Olivier Blanvillain
    10  Liu Fengyun
     8  Aleksander Boruch-Gruszecki
     6  Tobias Bordenca
     5  Sébastien Doeraene
     4  Stéphane Micheloud
     3  João Pedro Evangelista
     3  Miles Sabin
     3  Neeraj Jaiswal
     3  Abel Nieto
     2  Ólafur Páll Geirsson
     2  Fengyun Liu
     2  veera venky
     1  mikhail
     1  Glavo
     1  0xflotus
     1  Paolo G. Giarrusso
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
