---
layout: blog-page
title: Announcing Dotty 0.6.0 and 0.7.0-RC1
author: Allan Renucci
authorImg: /images/allan.jpg
date: 2018-03-05
---

Today, we are excited to release Dotty versions 0.6.0 and 0.7.0-RC1. These releases
serve as a technology preview that demonstrates new language features and the compiler supporting them.

If you’re not familiar with Dotty, it's a platform to try out new language concepts and compiler
technologies for Scala. The focus is mainly on simplification. We remove extraneous syntax
(e.g. no XML literals), and try to boil down Scala’s types into a smaller set of more fundamental
constructs. The theory behind these constructs is researched in
[DOT](https://infoscience.epfl.ch/record/215280), a calculus for dependent object types.
You can learn more about Dotty on our [website](http://dotty.epfl.ch).

<!--more-->

This is our seventh scheduled release according to our [6-week release schedule](/docs/usage/version-numbers.html).
The [previous technology preview](https://github.com/lampepfl/dotty/releases/tag/0.6.0-RC1) focussed
on bug fixes and stability work.

## What’s new in the 0.7.0-RC1 technology preview?

### Enums Simplicification [#4003](https://github.com/lampepfl/dotty/pull/4003)
The previously introduced syntax and rules for enum were arguably too complex. We can considerably
simplify them by taking away one capability: that cases can have bodies which can define members.
Arguably, if we choose an ADT decomposition of a problem, it's good style to write all methods using
pattern matching instead of overriding individual cases. So this removes an unnecessary choice.
We now treat enums unequivocally as classes. They can have methods and other statements just like
other classes can. Cases in enums are seen as a form of constructors. We do not need a
distinction between enum class and enum object anymore. Enums can have companion objects just like
normal classes can, of course.

Let's consider how `Option` can be represented as an enum. Previously using an enum class:
```scala
enum class Option[+T] {
   def isDefined: Boolean
}
object Option {
  case Some[+T](x: T) {
     def isDefined = true
  }
  case None {
     def isDefined = false
  }

  def apply[T(x: T): Option[T] = if (x == null) None else Some(x)
}
```

And now:
```scala
enum Option[+T] {
  case Some(x: T)
  case None

  def isDefined: Boolean = this match {
    case None => false
    case some => true
  }
}

object Option {
  def apply[T](x: T): Option[T] = if (x == null) None else Some(x)
}
```

You can visit our website for more information about [enumerations](/docs/reference/enums/enums.html)
and how we can use them to model [Algebraic Data Types](/docs/reference/enums/adts.html).

### Ghost terms [#3342](https://github.com/lampepfl/dotty/pull/3342) and remove Phantom types [#3410](https://github.com/lampepfl/dotty/pull/3410)
The keyword `ghost` can be placed on parameters, `val` and `def` to enforce that no reference to
those terms is ever used (recursively). As they are never used, they can safely be removed during compilation.
These have similar semantics as _phantom types_ but with the added advantage that any type can be an ghost parameter. They can be used to add implicit type constraints that are only relevant at compilation time.

```scala
// A function that requires an implicit evidence of type X =:= Y but never uses it.
// The parameter will be removed and the argument will not be evaluated.
def apply(implicit ghost ev: X =:= Y) =
  foo(ev) // `ev` can be an argument to foo as foo will also never use it
def foo(ghost x:  X =:= Y) = ()
```

The previous code will be transformed to the following:

```scala
def apply() = // ghost parameter will be removed
  foo() // foo is called without the ghost parameter
def foo() = () // ghost parameter will be removed
```

[Documentation](/docs/reference/ghost-terms.html)


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
the [IDE guide](http://dotty.epfl.ch/docs/usage/ide-support.html).


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

According to `git shortlog -sn --no-merges 0.6.0-RC1..0.7.0-RC1` these are:

```
TODO
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](http://dotty.epfl.ch/docs/contributing/getting-started.html),
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
[@Duhemm]: https://github.com/duhemm

