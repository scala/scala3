---
layout: blog-page
title: Announcing Dotty 0.16.0-RC3 – the Scala Days 2019 Release
author: Aggelos Biboudis and Anatolii Kmetiuk
date: 2019-06-11
---

Hello again! Today, we are excited to announce the 16th release of Dotty. The
development of Dotty continues according to our schedule but today, Tuesday June
the 11th, we are electrified as it is the first day of [Scala Days 2019](https://scaladays.org/)
which marks the *10th* anniversary of Scala Days.
With this release we are getting closer to the _envelope_ of the new features
that Dotty plans to offer.

![]({{ site.baseurl }}/images/others/scala-days-logo.png "Scala Days 2019")

This release serves as a technology preview that demonstrates new
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

This is our 16th scheduled release according to our
[6-week release schedule](https://nightly.scala-lang.org/docs/contributing/procedures/release.html).

# What’s new in the 0.16.0-RC3 technology preview?

## Syntax Change: Type Lambdas

We reconsider the syntax of type lambdas in an effort to provide an improved
visual cue for two categories of types: types that relate to normal function
types and types that operate on a higher level. The _fat_ arrow `=>` definitely
relates to the first, while we reserve now `->` to mean _pure function_ in the
future. As a result, we disengage `=>` from type lambdas, which are now
represented by `=>>`. As a result a function from types to types is written as
`[X] =>> F[X]`.

For those who are interested in the discussions,
[#6558](https://github.com/scala/scala3/pull/6558) introduced the new syntax.

## Syntax Change: Wildcard Arguments in Types

The syntax of wildcard arguments in types has changed from `_` to `?`. Example:

```scala
List[?]
Map[? <: AnyRef, ? >: Null]
```

Again, in an effort to fine-tune our syntax we put two features, from the world
of terms and types, side-by-side and drew parallels at the syntactic level.
Consequently, as `f(_)` is a shorthand for the lambda `x => f(x)` and as we plan
ahead for making `C[_]` to be a shorthand for the type lambda `[X] =>> C[X]` in
the future we pick `?` as a replacement syntax for wildcard types, since it
aligns with Java's syntax.

For more information please read our documentation on
[Wildcards](https://nightly.scala-lang.org/docs/reference/changed-features/wildcards.html).

## Syntax Change: Contextual Abstractions

We reconsider the syntax for contextual abstractions introducing `delegates`
(formerly known as `implied`). `delegate`, in the context of contextual
abstraction means that we declare a _representative of a type_. We use
`delegate` as a noun. Note that this change is solely syntactical/grammatical
and its motivation is to give a clearer meaning to those _canonical_ values of
certain types (like `Ord[Int]`), that serve for synthesizing arguments to
`given` clauses.

```scala
delegate IntOrd for Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}
```

```scala
delegate ListOrd[T] for Ord[List[T]] given (ord: Ord[T]) {
```

For more information, the documentation has been updated as part of the relevant
PR [#6649](https://github.com/scala/scala3/pull/6649)

## Polymorphic function types

We add preliminary support for _polymorphic function types_. Nowadays, when we
want to write a universally quantified function over elements of lists of type
`T` we write e.g., `List[T] => List[(T, T)]` where `T` is bound at an enclosing
definition. With polymorphic function types (PFT hereafter) we can quantify the
parametric type locally. For example:

```scala
[T <: AnyVal] => List[T] => List[(T, T)]
```

As you notice, this gives us the ability to impose restrictions on the type
variable `T` locally. Assume, you have an identity function with `type id = T => T`.
By writing it as `type id = [T] => T => T` we abstract further the concept
of a _polymorphic function_ and make it a *true* _family of functions_.

The code below (correctly) fails to type check because `T` needs to be bounded
in the enclosing class:

```scala
  val id: T => T = t => t
  println(s"${id(1)} , ${id(7.0d)}")
```

With PFTs we can now achieve what we want:

```scala
  val id = [T] => (t: T) => t
  println(s"${id(1)} , ${id(7.0d)}")
```

For those who are interested in the discussions and more test cases,
[#4672](https://github.com/scala/scala3/pull/4672/) introduced PFTs.

## `lazy val`s are now thread-safe by default

Previously thread-safety was required using `@volatile` but that would not be
consistent with Scala 2. The old behavior of non-volatile lazy vals can be
recovered by using the newly-introduced `@threadUnsafe`.

For more information please read our documentation on the
[threadUnsafe annotation](https://nightly.scala-lang.org/docs/reference/other-new-features/threadUnsafe-annotation.html).

## Add support for Java-compatible enums

We add support for Java-compatible enumerations. The users can just extend
`java.lang.Enum[T]`.

```scala
enum A extends java.lang.Enum[A] {
  case MONDAY, TUESDAY, SATURDAY
}

enum B(val gravity: Double) extends java.lang.Enum[B] {
  case EARTH extends B(9.8)
  case JUPITER extends B(100)
  case MOON extends B(4.3)
  case Foo extends B(10)
}
```

For more information please check the [test case](https://github.com/scala/scala3/tree/main/tests/run/enum-java) and
also the relevant PRs [#6602](https://github.com/scala/scala3/pull/6602) and
[#6629](https://github.com/scala/scala3/pull/6629).

In the test, the enums are defined in the `MainScala.scala` file and used from a
Java source, `Test.java`.

## Introducing `for` clauses for importing delegate imports by type

Since delegate instances can be anonymous it is not always practical to import
them by their name, and wildcard imports are typically used instead. By-type
imports provide a more specific alternative to wildcard imports, which makes it
clearer what is imported. Example:

 ```scala
import delegate A.{for TC}
```

This imports any delegate instance in `A` that has a type which conforms tp `TC`.
There can be several bounding types following a `for` and bounding types can
contain wildcards.
For instance, assuming the object

```scala
object Delegates {
  delegate intOrd for Ordering[Int]
  delegate [T: Ordering] listOrd for Ordering[List[T]]
  delegate ec for ExecutionContext = ...
  delegate im for Monoid[Int]
}
```
the import
```scala
import delegate Delegates.{for Ordering[_], ExecutionContext}
```
would import the `intOrd`, `listOrd`, and `ec` instances but leave out the `im`
instance, since it fits none of the specified bounds.

## New type class derivation scheme

Summary of measured differences with the old scheme:

- About 100 lines more compiler code - the rest of the lines changed diff is
tests.
- About 13-15% more code generated for type class instances
- About 3-4% slower to compile type class instances

Advantages of new scheme:

- Fewer allocations, since mirrors (`Generic` has been renamed to `Mirror`) are
  usually shared instead of being allocated at runtime.
- It works well even if there are no derives clauses. The old scheme would
  generate more code in that case.
- Complete decoupling between derives clauses and mirror generation.

For the technical details of these changes please consule the corresponding PR
[#6531](https://github.com/scala/scala3/pull/6531).

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.15.0-RC1..0.16.0-RC3` these are:

```
88  Martin Odersky
51  Anatolii
48  Nicolas Stucki
26  Guillaume Martres
21  Miles Sabin
19  Liu Fengyun
12  Aleksander Boruch-Gruszecki
11  Sébastien Doeraene
 8  Aggelos Biboudis
 4  Olivier Blanvillain
 3  Eugene Yokota
 1  Dale Wijnand
 1  Allan Renucci
 1  Olivier ROLAND
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
[@anatoliykmetyuk]: https://github.com/anatoliykmetyuk
