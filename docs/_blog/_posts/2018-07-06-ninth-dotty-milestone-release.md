---
layout: blog-page
title: Announcing Dotty 0.9.0-RC1
author: Allan Renucci
authorImg: images/allan.jpg
date: 2018-07-06
---

Today, we are excited to release Dotty version 0.9.0-RC1. This release serves as a technology
preview that demonstrates new language features and the compiler supporting them.

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

This is our ninth scheduled release according to our [6-week release schedule](https://dotty.epfl.ch/docs/usage/version-numbers.html).
The [previous technology preview](https://github.com/lampepfl/dotty/releases/tag/0.8.0-RC1) added
support for sbt 1, introduced improved unchecked warnings and improved SAM type support.

## What’s new in the 0.9.0-RC1 technology preview?

### Improved REPL [#4680](https://github.com/lampepfl/dotty/pull/4680)
The REPL now uses [JLine 3](https://github.com/jline/jline3) under the hood which improves on
many aspects such as, auto-completions and multi-line editing. The REPL now also works on Windows!


### Documentation support in the IDE [#4461](https://github.com/lampepfl/dotty/pull/4461), [#4648](https://github.com/lampepfl/dotty/pull/4648)
The Dotty IDE will now display documentation while hovering over symbols that were previously
compiled by the Dotty compiler. In the future, we plan to let users query the documentation
in the REPL as well.


### Drop requirement that implicit functions must be non-empty [#4549](https://github.com/lampepfl/dotty/pull/4549)
We remove the arbitrary restriction that parameters of implicit functions must by non-empty.
We can now write:
```scala
type IntProducer = implicit () => Int

def prod1: IntProducer = 1
val prod2: IntProducer = 2
```

An interesting observation is that by-name parameters can now be encoded as implicit function types:
```scala
def timed[T](op: => T): T = ...
def timed[T](op: implicit () => T): T = ...

timed {
  fetch(url)
}
```

Both definitions above are equivalent.


### Emit feature warnings for implicit conversions [#4229](https://github.com/lampepfl/dotty/pull/4229)
Implicit conversions are easily the most misused feature in Scala. We now emit feature warnings
when encountering an implicit conversion definition, just like Scala 2 does.

In addition, we also emit a feature warning when an implicit conversion is used,
unless the conversion is:

- an implicit class
- co-defined with the type to which it converts
- predefined in `scala.Predef` or is the `scala.reflect.Selectable.reflectiveSelect` conversion
  (we might extend this to more conversions).


### Optimise s and raw interpolators [#3961](https://github.com/lampepfl/dotty/pull/3961)
`s` and `raw` string interpolators were known to be slower than their not type-safe counterparts:
```scala
s"Hello $name!"

// compared to:
"Hello " + name + "!"
```
The compiler will now desugar the former into the latter. Special thanks to
[Wojtek Swiderski](https://github.com/Wojtechnology) who contributed this feature to the Dotty
compiler!


### Support for compiler plugins [#3438](https://github.com/lampepfl/dotty/pull/3438)
Dotty now supports Compiler plugins. Compiler plugins let you customize the compiler pipeline
without having to modify the compiler source code. A major difference compared to Scala 2 is
that Dotty plugins must run after the type checker. Being able to influence normal type checking
is a powerful feature but for production usages, a predictable and consistent type checker is
more important.

Starting with 1.1.5 Dotty compiler plugins can be used with `sbt`. Please refer to the `sbt`
[documentation](https://www.scala-sbt.org/1.x/docs/Compiler-Plugins.html) for more information.

For more information, visit the [Compiler Plugin](https://dotty.epfl.ch/docs/reference/changed-features/compiler-plugins.html)
section of our documentation.

## Trying out Dotty

### sbt
Using sbt 1.1.5 or newer, do:

```shell
sbt new lampepfl/dotty.g8
```

This will setup a new sbt project with Dotty as compiler. For more details on
using Dotty with sbt, see the
[example project](https://github.com/lampepfl/dotty-example-project).

### IDE support
Start using the Dotty IDE in any Dotty project by following
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

### Scastie
[Scastie], the online Scala playground, supports Dotty. This is an easy way to try Dotty without
installing anything. Note however that Scastie only supports Dotty 0.7.0-RC1.

## Let us know what you think!
If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing
Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.8.0..0.9.0-RC1` these are:

```
   200  Nicolas Stucki
   155  Martin Odersky
    71  Allan Renucci
    42  Paolo G. Giarrusso
    27  Aggelos Biboudis
    25  Guillaume Martres
    22  Martin Duhem
    10  Sebastian Nadorp
    10  Wojtek Swiderski
     6  Olivier Blanvillain
     5  benkobalog
     4  Ingar Abrahamsen
     3  Ankit Soni
     2  Liu Fengyun
     2  Olivier ROLAND
     2  Fabian Page
     1  Roberto Bonvallet
     1  Fengyun Liu
     1  Zoltán Elek
     1  benkbalog
     1  Glavo
     1  dieutth
     1  fschueler
     1  mentegy
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
