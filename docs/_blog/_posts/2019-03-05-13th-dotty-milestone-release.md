---
layout: blog-page
title: Announcing Dotty 0.13.0-RC1 with Spark support, top level definitions and redesigned implicits
author: Aggelos Biboudis
authorImg: images/aggelos.jpg
date: 2019-03-05
---

Hello hello! This is the second release for 2019. Spark, top level definitions
and redesigned implicits ✨🎊🎉 are the most important inclusions in this release
and you will understand why we are super excited, in a bit!

Without further ado, today we release the version 0.13.0-RC1 of the Dotty
compiler. This release serves as a technology preview that demonstrates new
language features and the compiler supporting them.

Dotty is the project name for technologies that are being considered for
inclusion in Scala 3. Scala has pioneered the fusion of object-oriented and
functional programming in a typed setting. Scala 3 will be a big step towards
realising the full potential of these ideas. Its main objectives are to

- become more opinionated by promoting programming idioms we found to work well,
- simplify where possible,
- eliminate inconsistencies and surprising behaviours,
- build on strong foundations to ensure the design hangs together well,
- consolidate language constructs to improve the language’s consistency, safety, ergonomics, and
  performance.

You can learn more about Dotty on our [website](https://nightly.scala-lang.org).

<!--more-->

This is our 13th scheduled release according to our
[6-week release schedule](https://nightly.scala-lang.org/docs/usage/version-numbers.html).

# What’s new in the 0.13.0-RC1 technology preview?

## Experimental support for Spark

Dotty projects have always been able to [depend on Scala 2
libraries](https://github.com/lampepfl/dotty-example-project#getting-your-project-to-compile-with-dotty),
and this usually works fine (as long as the Dotty code does not call a Scala 2
macro directly). However, [Spark](http://spark.apache.org/) was known to not work
correctly as it heavily relies on Java serialization which we were not fully
supporting.

Meanwhile, at EPFL, we've started updating our Scala courses to use Dotty
instead of Scala 2, the *Functional Programming* course given last semester went
smoothly, but the *Parallelism and Concurrency* course given in the
Spring semester teaches Spark, which means we needed to support it in Dotty!

Luckily, this turned out to be mostly straightforward: we adopted the [object
serialization scheme](https://github.com/scala/scala3/pull/5775) and [lambda
serialization scheme](https://github.com/scala/scala3/pull/5837) pioneered by
Scala 2, and that was enough to make our Spark assignments run correctly! This
doesn't mean that our support is perfect however, so don't hesitate to [open an
issue](http://github.com/scala/scala3/issues) if something is amiss.

## Introducing top level definitions

_Top level_ definitions are now supported. This means that package objects are
now redundant, and will be phased out. This means that all kinds of definitions
can be written at the top level.

```scala
package p

type Labelled[T] = (String, T)

val a: Labelled[Int] = ("count", 1)
def b = a._2
```

You can read about [dropping package
objects](https://nightly.scala-lang.org/docs/reference/dropped-features/package-objects.html)
at the documentation linked or at the relevant PR
[#5754](https://github.com/scala/scala3/pull/5754).

## All things impl... implied

Scala's implicits are its most distinguished feature. They are _the_ fundamental
way to abstract over context. They represent a single concept with an extremely
varied number of use cases, among them: implementing type classes, establishing
context, dependency injection, expressing capabilities, computing new types and
proving relationships between them.

However, with great power comes great responsibility. The current design of
implicits has shown some limitations, which we have been trying to identify and
address to make Scala a clearer and more pleasant language. First of all, we
found that the syntactic similarity was too great between implicit _conversions_
and implicit _values_ that depend on other implicit values. Both of them appear
in the snippet below:

```scala
implicit def i1(implicit x: T): C[T] = ... // 1: conditional implicit value
implicit def i2(x: T): C[T] = ...          // 2: implicit conversion
```

Some users used implicit conversions, in an unprincipled matter. This overuse of
implicit conversions decluttered code. However, while implicit conversions can
be useful to remove clutter, their abuse makes it harder for people to reason
about the code.

The `implicit` keyword is used for both implicit conversions and conditional
implicit values and we identified that their semantic differences must be
communicated more clearly syntactically. Furthermore, the `implicit` keyword is
ascribed too many overloaded meanings in the language (implicit vals, defs,
objects, parameters). For instance, a newcomer can easily confuse the two
examples above, although they demonstrate completely different things, a
type class instance is an implicit object or val if unconditional and an implicit
def with implicit parameters if conditional; arguably all of them are
surprisingly similar (syntactically). Another consideration is that the
`implicit` keyword annotates a whole parameter section instead of a single
parameter, and passing an argument to an implicit parameter looks like a regular
application. This is problematic because it can create confusion regarding what
parameter gets passed in a call. Last but not least, sometimes implicit
parameters are merely propagated in nested function calls and not used at all,
so giving names to implicit parameters is often redundant and only adds noise to
a function signature.

Consequently, we introduce two new language features:

1. _implied instance definitions_ designated syntactically by the scheme `implied ... for` and,
2. _inferable parameters_ designated by the keyword `given`.

In the code below we demonstrate both of them. This code defines a trait `Ord`
and two `implied` instance definitions. `IntOrd` defines an `implied` instance
for the type `Ord[Int]` whereas `ListOrd[T]` defines implied instances of
`Ord[List[T]]` for all types `T` that come with an implied `Ord[T]` instance
themselves. The `given` clause in `ListOrd` defines an _inferable parameter_.

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

implied IntOrd for Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

implied ListOrd[T] given (ord: Ord[T]) for Ord[List[T]] {
  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match {
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = ord.compare(x, y)
      if (fst != 0) fst else xs1.compareTo(ys1)
  }
}
```

A `given` clause can also designate an inferable parameter for functions:

```scala
def max[T](x: T, y: T) given (ord: Ord[T]): T =
  if (ord.compare(x, y) < 1) y else x
```

With this scheme all invocations of the `max` function below are equally valid:
```scala
max(2, 3) given IntOrd
max(List(1, 2, 3), Nil)
max(2, 3)
```

We introduce _Anonymous Implied Instances_ which are used when we do not need a name for an implied
instance:

```scala
implied for Ord[Int] { ... }
```

For convenience, we also introduce _Implied Alias Instances_. They offer aliases
for implied instances. For example, the line below offers an alias with the name
`ctx` (could also be anonymous if name can be omitted). Each time an implied
instance of `ExecutionContext` is demanded the right-hand side is returned.

```scala
implied ctx for ExecutionContext = currentThreadPool().context
```

We have also added a synonym to `implicitly`, which is often more natural to
spell out in user code. Functions like `the` that have only _inferable
parameters_ are also called _context queries_ from now on. Consequently, to
summon an implied instance of `Ord[List[Int]]` we write:

```scala
the[Ord[List[Int]]]
```

How do we import implied instances in scope? From now on, normal import clauses
fetch all definitions *except* implied instance into scope whereas _Implied
Imports_ bring only implied instances in scope.

```scala
object A {
  class TC
  implied tc for TC
  def f given TC = ???
}
object B {
  import A._         // normal import clause
  import implied A._ // implied import clause
}
```

**You can read more about** [implied
imports](https://nightly.scala-lang.org/docs/reference/contextual/import-delegate.html)
from the docs or the relevant PR
[#5868](https://github.com/scala/scala3/pull/5868).

As we mentioned above, *context queries* are functions with (only) inferable
parameters. Here is an example of such a function:

```scala
type Contextual[T] = given Context => T
```

Context queries--previously named implicit function types (IFTs)--are now also
expressed with `given`, providing types for first-class context queries. This is
merely an alignment of IFTs into the new scheme.

**You can read more about** the alternative to implicits through the *Contextual
Abstractions* section of our documentation or for a deep dive from the relevant
PR chain that originated from
[#5458](https://github.com/scala/scala3/pull/5458). The syntax changes for new
implicits are summarized in
[#5825](https://github.com/scala/scala3/pull/5825).

This release offers the support for _type class derivation_ as a language
feature. Type class derivation is a way to generate instances of certain type
classes automatically or with minimal code hints, and is now supported natively
with *dedicated language support*. A type class in this sense is any trait or
class with a type parameter that describes the type being operated on. Commonly
used examples are `Ordering`, `Show`, or `Pickling`. We introduce a new
`derives` clause that generates implied instances of the `Eql`, `Ordering`, and
`Pickling` traits in the companion object `Tree`. Take a look at the example
below:

```scala
enum Tree[T] derives Eql, Ordering, Pickling {
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)
}
```

where the generated implied instances are the ones below:
```scala
implied [T: Eql]      for Eql[Tree[T]]      = Eql.derived
implied [T: Ordering] for Ordering[Tree[T]] = Ordering.derived
implied [T: Pickling] for Pickling[Tree[T]] = Pickling.derived
```

Note, the new syntax:

```scala
A extends B, C { ... }
```

which replaces:

```scala
A extends B with C { ... }
```

With type class derivation we can also derive types. A trait or class can appear
in a derives clause if its companion object defines a method named `derived`.
The type and implementation of a `derived` method are arbitrary, but typically
it has a definition like this:

```scala
def derived[T] given Generic[T] = ...
```

**You can read more about** [Type class
Derivation](https://nightly.scala-lang.org/docs/reference/contextual/derivation.html) or
have a deep dive at the relevant PRs:
[#5540](https://github.com/scala/scala3/pull/5540) and
[#5839](https://github.com/scala/scala3/pull/5839).

_Multiversal equality_ is now supported through the `Eql` marker trait (renamed
from `Eq` to differentiate it from Cats' `Eq`). For example, in order to be able
to compare integers with strings now, instead of a custom implicit we can
provide a derived implicit instance:

```scala
implied for Eql[Int, String] = Eql.derived
```

**You can read more about** how we based multiversal equality on type class derivation through
the relevant PR [#5843](https://github.com/scala/scala3/pull/5843).

_Implicit conversions_ are now defined by implied instances of the
`scala.Conversion` class. For example:

```scala
implied for Conversion[String, Token] {
  def apply(str: String): Token = new KeyWord(str)
}
```

**Note:** that these release notes contain only a brief summary of the new
features, for more details please read our documentation page under the new
section named [*Contextual Abstractions*](https://nightly.scala-lang.org/docs/). Equally
important with the documentation of each feature, please consult the
[Relationship with Scala 2 Implicits](https://nightly.scala-lang.org/docs/reference/contextual/relationship-implicits.html) section as well.

## Implicit resolution rule changes

PR [#5887](https://github.com/scala/scala3/pull/5887) applies the following
changes to implicit resolution:

1. nested implicits always take precedence over outer ones
2. no more shadowing checks
3. package prefixes are not considered.

## SemanticDB generator

[SemanticDB](https://github.com/scalameta/scalameta/tree/master/semanticdb) is a
data model for semantic information such as symbols and types about programs in
Scala and other languages. SemanticDB decouples production and consumption of
semantic information, establishing documented means for communication between
tools. With PR [#5761](https://github.com/scala/scala3/pull/5761) we add the
first prototype for the generation of SemanticDB information from TASTy.

## And much more!

Please read our [release notes](https://github.com/scala/scala3/releases/tag/0.13.0-RC1)
for more details!

# Trying out Dotty

## sbt

You can set up a new sbt project with Dotty as the compiler by running:

```shell
sbt new lampepfl/dotty.g8
```

For more details on using Dotty with sbt, see the
[example project](https://github.com/lampepfl/dotty-example-project).

<!-- ## [Mill](http://www.lihaoyi.com/mill/)

The Mill build tool version 0.2.6 introduced experimental support for Dotty. For more details on
using Dotty with Mill, see the
[example project](https://github.com/lampepfl/dotty-example-project/tree/mill). -->

## IDE support

Start using the Dotty IDE in any Dotty project by following
the IDE sections of the [getting-started page](https://docs.scala-lang.org/scala3/getting-started.html).

## Standalone installation

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

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.12.0-RC1..0.13.0-RC1` these are:

```
   309  Martin Odersky
   116  Nicolas Stucki
    52  Guillaume Martres
    42  poechsel
    22  Aggelos Biboudis
    20  Paolo G. Giarrusso
    19  Olivier Blanvillain
    11  Liu Fengyun
     5  Allan Renucci
     4  Miles Sabin
     3  Tobias Bordenca
     3  Lionel Parreaux
     3  Abel Nieto
     2  Lukas Rytz
     1  lpwisniewski
     1  Adriaan Moors
     1  Georg Schmid
     1  Jentsch
     1  Marc Karassev
     1  Daniel Murray
     1  Olivier ROLAND
     1  Raphael Jolly
     1  Stéphane Micheloud
     1  Sébastien Doeraene
     1  Umayah Abdennabi
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
