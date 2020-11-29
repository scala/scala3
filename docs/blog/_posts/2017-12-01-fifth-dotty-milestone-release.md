---
layout: blog-page
title: Announcing Dotty 0.5.0-RC1
author: Allan Renucci
authorImg: images/allan.jpg
date: 2017-12-01
---

Today, we are excited to release Dotty version 0.5.0-RC1. This release
serves as a technology preview that demonstrates new language features
and the compiler supporting them.

If you’re not familiar with Dotty, it's a platform to try out new language concepts and compiler
technologies for Scala. The focus is mainly on simplification. We remove extraneous syntax
(e.g. no XML literals), and try to boil down Scala’s types into a smaller set of more fundamental
constructs. The theory behind these constructs is researched in
[DOT](https://infoscience.epfl.ch/record/215280), a calculus for dependent object types.
You can learn more about Dotty on our [website](http://dotty.epfl.ch).

<!--more-->

This is our fifth scheduled release according to our [6-week release schedule](https://dotty.epfl.ch/docs/usage/version-numbers.html).
The [previous technology preview](/blog/_posts/2017-10-16-fourth-dotty-milestone-release.html) added
support for Scala 2.12 and came with a brand new REPL.

## What’s new in the 0.5.0-RC1 technology preview?

### Reworked implicit search [#3421](https://github.com/lampepfl/dotty/pull/3421)
The treatment of ambiguity errors has changed. If an ambiguity is encountered
in some recursive step of an implicit search, the ambiguity is propagated to the caller.
Example: Say you have the following definitions:

```scala
class A
class B extends C
class C
implicit def a1: A
implicit def a2: A
implicit def b(implicit a: A): B
implicit def c: C
```

and the query `implicitly[C]`.

This query would now be classified as ambiguous. This makes sense, after all
there are two possible solutions, `b(a1)` and `b(a2)`, neither of which is better
than the other and both of which are better than the third solution, `c`.
By contrast, Scala 2 would have rejected the search for `A` as
ambiguous, and subsequently have classified the query `b(implictly[A])` as a normal fail,
which means that the alternative `c` would be chosen as solution!

Scala 2's somewhat puzzling behavior with respect to ambiguity has been exploited to implement
the analogue of a "negated" search in implicit resolution, where a query `Q1` fails if some other
query `Q2` succeeds and `Q1` succeeds if `Q2` fails. With the new cleaned up behavior these
techniques no longer work. But there is now a new special type `scala.implicits.Not` which
implements negation directly. For any query type `Q`: `Not[Q]` succeeds if and only if the
implicit search for `Q` fails.

### Dependent function types [#3464](https://github.com/lampepfl/dotty/pull/3464)
A dependent function type describes functions where the result type may depend
on the function's parameter values. Example:

```scala
class Entry { type Key; key: Key }

def extractKey(e: Entry): e.Key = e.key          // a dependent method
val extractor: (e: Entry) => e.Key = extractKey  // a dependent function value
```

Scala already has _dependent methods_, i.e. methods where the result
type refers to some of the parameters of the method. Method
`extractKey` is an example. Its result type, `e.key` refers its
parameter `e` (we also say, `e.Key` _depends_ on `e`). But so far it
was not possible to turn such methods into function values, so that
they can be passed as parameters to other functions, or returned as
results. Dependent methods could not be turned into functions simply
because there was no type that could describe them.

In Dotty this is now possible. The type of the `extractor` value above is

```scala
(e: Entry) => e.Key
```

This type describes function values that take any argument `x` of type
`Entry` and return a result of type `x.Key`.

### TASTY frontend
[TASTY](https://docs.google.com/document/d/1Wp86JKpRxyWTqUU39H40ZdXOlacTNs20aTj7anZLQDw/edit) is a
new serialization format for typed syntax trees of Scala programs. When compiled by Dotty, a program
classfile will include its TASTY representation in addition to its bytecode.

The TASTY frontend uses ASTs from the TASTY in classfiles as input instead of source files. There
are currently two backends using the TASTY frontend:

 - A Dotty class file decompiler that let you decompile code previously compiled to TASTY:

   ```shell
   dotc -decompile -classpath <classpath> <classname>
   ```

 - A Dotty TASTY compiler that will recompile code previously compiled to TASTY:

   ```shell
   dotc -from-tasty -classpath <classpath> <classname>
   ```

   This is the first step toward linking and whole word optimisations, recompiling code to a
   different backends...

### Generic java signatures [#3234](https://github.com/lampepfl/dotty/pull/3234)
Dotty now emits generic signatures for classes and methods. Those signatures are used by compilers,
debuggers and to support runtime reflection. For example:

```scala
scala> class Foo[T, U]
// defined class Foo

scala> classOf[Foo[_, _]].getTypeParameters.map(_.getName).mkString(", ")
val res0: String = "T, U"
```

## Trying out Dotty
### Scastie
[Scastie], the online Scala playground, supports Dotty.
This is an easy way to try Dotty without installing anything.

### sbt
Using sbt 0.13.13 or newer, do:

```shell
sbt new lampepfl/dotty.g8
```

This will setup a new sbt project with Dotty as compiler. For more details on
using Dotty with sbt, see the
[example project](https://github.com/lampepfl/dotty-example-project).

### IDE support
It is very easy to start using the Dotty IDE in any Dotty project by following
the [IDE guide](https://dotty.epfl.ch/docs/usage/ide-support.html).


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

According to `git shortlog -sn --no-merges 0.4.0-RC1..0.5.0-RC1` these are:

```
   112  Nicolas Stucki
   108  Martin Odersky
    33  Allan Renucci
    18  Guillaume Martres
    17  Martin Duhem
    13  liu fengyun
     9  Miron Aseev
     4  Matt D'Souza
     4  Raphael Bosshard
     2  k0ala
     2  Vitor Vieira
     2  Fengyun Liu
     2  Michal Gutowski
     2  Robert Soeldner
     2  Aurélien Richez
     1  rsoeldner
     1  Hermes Espínola González
     1  Jean Detoeuf
     1  Karol Chmist
     1  Olivier Blanvillain
     1  William Narmontas
     1  Yevgen Nerush
     1  gan74
     1  gosubpl
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
You can have a look at our [Getting Started page for new contributors](https://dotty.epfl.ch/docs/contributing/getting-started.html),
the [Awesome Error Messages](http://scala-lang.org/blog/2016/10/14/dotty-errors.html) project or some of
the simple [Dotty issues](https://github.com/lampepfl/dotty/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
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
