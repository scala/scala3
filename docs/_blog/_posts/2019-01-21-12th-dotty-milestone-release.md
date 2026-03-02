---
layout: blog-page
title: Announcing Dotty 0.12.0-RC1
author: Aggelos Biboudis
authorImg: images/aggelos.jpg
date: 2019-01-21
---

Happy New Year to all with the first release of Dotty for 2019! ✨🎊🎉

Today we are excited to release the version 0.12.0-RC1 of the Dotty compiler.
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

You can learn more about Dotty on our [website](https://nightly.scala-lang.org).

<!--more-->

This is our 12th scheduled release according to our
[6-week release schedule](https://nightly.scala-lang.org/docs/usage/version-numbers.html).

## What’s new in the 0.12.0-RC1 technology preview?

### Extension Methods

We are excited to announce that extension methods are now offered through dedicated language support!
Extension methods allow one to add methods to a type after the type is defined.
This is done by writing a method with a parameter for the type to be extended
_on the left-hand side_ of the method name:

```scala
case class Circle(x: Double, y: Double, radius: Double)

def (c: Circle) circumference: Double = c.radius * math.Pi * 2
```

Extension methods are enabled when they are syntactically in scope (as above),
or when their enclosing instance is present in the implicit scope of the type that they extend,
as we exemplify below.

Extension methods were previously encoded in a rather roundabout way via the implicit class pattern.
Such encoding required a lot of boilerplate, especially when defining type classes.
In Dotty, this is no longer the case,
and type classes with infix syntax become very straightforward to define!
For example, consider:

```scala
trait Semigroup[T] {
  def (x: T) combine (y: T): T
}
implicit val IntSemigroup: Semigroup[Int] = new {
   def (x: Int) combine (y: Int): Int = x + y
}
implicit def ListSemigroup[T]: Semigroup[List[T]] = new {
   def (x: List[T]) combine (y: List[T]): List[T] = x ::: y
}
1.combine(2) // == 3
List(1,2).combine(List(3,4)) // == List(1,2,3,4)
```

This works because the `combine` extension methods of `IntSemigroup` and `ListSemigroup` are available
from the relevant implicit scopes.

Read the [full documentation](https://nightly.scala-lang.org/docs/reference/contextual/extension-methods.html) about generic extension methods, higher-kinded extension methods, and more.

### TASTy Reflect goodies

We implement a new decompiler for TASTy files and we also offer a new VS Code Extension.
The decompiler allows to view both decompiled scala source code and the pretty printed TASTy tree when opening a .tasty file.
The feature is similar to opening .class files in IntelliJ.

![]({{ site.baseurl }}/images/dotty-ide/decompiler.png "Decompiler")

The decompiler can be invoked with the corresponding flag: `dotc -decompile xyz.tasty`.

On the programmatic side of TASTy we are rolling out changes according to our plan discussed at [Macros: The Plan for Scala 3](https://www.scala-lang.org/blog/2018/04/30/in-a-nutshell.html).
In this release, we make progress following the _Next Steps_ of the aforementioned blogpost by offering constructors that work directly with reflect trees.
Consequently, TASTy extractors meet their dual, TASTy constructors!
We also connect the new lower-level reflection layer to the existing principled macro system based on quotes and splices offering, two new expression methods for `Expr[T]`:

- `unseal` that unseals an `Expr[T]` (non traversable code) into a `Term` and
- `seal` that seals back a `Term` into an `Expr[T]`.

Read the [relevant documentation](https://nightly.scala-lang.org/docs/reference/metaprogramming/reflection.html) to learn how to go from quotes and splices to TASTys Reflect trees and back .

### Alignments with the Scala Improvement Process

In this version we improve the implementation of by-name implicits making it compliant with the [By-name Implicits](https://docs.scala-lang.org/sips/byname-implicits.html) SIP and we implement the `ValueOf` operator which yields the unique value of types with a single inhabitant, effectively syncing it up with the [Literal-Based Singleton Types](https://docs.scala-lang.org/sips/42.type.html) SIP.

### Improvements to GADT support

In this release, we're also rolling out the first batch of improvements to GADT support - namely, variable unification.
To keep it short, from knowing that `A <: B` and `B <: A`, we can now deduce that `A = B`, and from `A = B` and `B <: C` we deduce that `A <: C`.
This kind of reasoning is necessary for many advanced GADT usages!

### And much more!

Please read our [release notes](https://github.com/scala/scala3/releases/tag/0.12.0-RC1)
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
[https://github.com/scala/scala3/releases](https://github.com/scala/scala3/releases)

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
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.11.0-RC1..0.12.0-RC1` these are:

```
   109  Martin Odersky
    64  Nicolas Stucki
    34  Martin Duhem
    25  Allan Renucci
    16  Guillaume Martres
    12  Aleksander Boruch-Gruszecki
    11  Tobias Bordenca
    10  Miles Sabin
    10  Liu Fengyun
     7  Aggelos Biboudis
     7  Jaemin Hong
     5  Paolo G. Giarrusso
     3  duanebester
     3  Dotty CI
     2  Sébastien Doeraene
     2  Saurabh Rawat
     2  Vlastimil Dort
     1  tOverney
     1  Denis Buzdalov
     1  Hermes Espínola González
     1  Ivan Youroff
     1  João Pedro de Carvalho
     1  Neeraj Jaiswal
     1  Olivier Blanvillain
     1  poechsel
     1  Abel Nieto
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://nightly.scala-lang.org/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/scala/scala3/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
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
[@AleksanderBG]: https://github.com/AleksanderBG
[@milessabin]: https://github.com/milessabin