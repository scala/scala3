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

This is our seventh scheduled release according to our [6-week release schedule](http://dotty.epfl.ch/docs/usage/version-numbers.html).
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

  def apply[T](x: T): Option[T] = if (x == null) None else Some(x)
}
```

And now:
```scala
enum Option[+T] {
  case Some(x: T)
  case None

  def isDefined: Boolean = this match {
    case None => false
    case Some(_) => true
  }
}

object Option {
  def apply[T](x: T): Option[T] = if (x == null) None else Some(x)
}
```


For more information about [Enumerations](http://dotty.epfl.ch/docs/reference/enums/enums.html)
and how to use them to model [Algebraic Data Types](http://dotty.epfl.ch/docs/reference/enums/adts.html),
visit the respective sections in our documentation.


### Ghost terms [#3342](https://github.com/lampepfl/dotty/pull/3342)
The `ghost` modifier can be used on parameters, `val` and `def` to enforce that no reference to
those terms is ever used. As they are never used, they can safely be removed during compilation.

One particular use case is to add implicit type constraints that are only relevant at compilation
time. For example, let's consider the following implementation of `flatten`.

```scala
class List[X] {
  def flatten[Y](implicit ghost ev: X <:< List[Y]): List[Y] = {
    val buffer = new mutable.ListBuffer[Y]
    this.foreach(e => buffer ++= e.asInstanceOf[List[Y]])
    buffer.toList
  }
}

List(List(1, 2), List(3)).flatten // List(1, 2, 3)
List(1, 2, 3).flatten             // error: Cannot prove that Int <:< List[Y]
```

The implicit evidence `ev` is only used to constrain the type parameter `X` of `List` such that we
can safely cast from `X` to `List[_]`. The usage of the `ghost` modifier ensures that the evidence
is not used and can be safely removed at compilation time.

For more information, visit the [Ghost Terms](http://dotty.epfl.ch/docs/reference/ghost-terms.html)
section of our documentation.

**Note**: Ghost terms replace _phantom types_: they have similar semantics, but with the added
advantage that any type can be a ghost parameter. See [#3410](https://github.com/lampepfl/dotty/pull/3410).


### Improved IDE support [#3960](https://github.com/lampepfl/dotty/pull/3960)
The Dotty language server now supports context sensitive IDE completions. Completions now include
local and imported definitions. Members completions take possible implicit conversions into account.

<!-- A GIF would be nice? -->

We also improved the `find references` functionality. It is more robust and much faster!

Try it out in [Visual Studio Code](http://dotty.epfl.ch/docs/usage/ide-support.html)!

### Improvements in GADT type inference

GADT typechecking is an advanced feature that got significantly improved in this
release. GADTs are case class hierarchies similar to this one:

```scala
sealed trait Exp[T]
case class IntLit(n: Int) extends Exp[Int]
case class BooleanLit(b: Boolean) extends Exp[Boolean]

case class GenLit[T](t: T) extends Exp[T]
case class Plus(e1: Exp[Int], e2: Exp[Int]) extends Exp[Int]
case class Fun[S, T](f: Exp[S] => Exp[T]) extends Exp[S => T]
case class App[T, U](f: Exp[T => U], e: Exp[T]) extends Exp[U]
```

where different constructors, such as `IntLit` and `BooleanLit`, pass different type argument to the super trait. Hence, typechecking a pattern match on `v: Exp[T]` requires special care: for instance, if `v = IntLit(5)` then `T` must be `Int`. This enables writing a typed interpreter `eval[T](e: Exp[T]): T`. In each pattern matching branch

```scala
object Interpreter {
  def eval[T](e: Exp[T]): T = e match {
    case IntLit(n) => // Here T = Int and n: Int
      n
    case BooleanLit(b) => // Similarly, here T = Boolean and b: Boolean
      b

    case GenLit(t) => //Here t: T

      // the next line is an error, but was allowed before the fix to https://github.com/lampepfl/dotty/issues/1754:
      //val w: GenLit[Nothing] = w

      t

    case Plus(e1, e2) =>
      // Here T = Int and e1, e2: Exp[Int]
      eval(e1) + eval(e2)

    // The next cases triggered warnings before the fix to
    // https://github.com/lampepfl/dotty/issues/3666

    case f: Fun[s, t]  => // Here T = s => t
      (v: s) => eval(f.f(GenLit(v)))

    case App(f, e)     => // Here f: Exp[s, T] and e: Exp[s]
      eval(f)(eval(e))
  }
}
```

Earlier Dotty releases had issues typechecking such interpreters.
We have fixed multiple bugs about GADT type checking and exhaustiveness checking, especially for invariant GADTs, including
[#3666](https://github.com/lampepfl/dotty/issues/3666),
[#1754](https://github.com/lampepfl/dotty/issues/1754),
[#3645](https://github.com/lampepfl/dotty/issues/3645),
and improved handling of matches using repeated type variables
[#4030](https://github.com/lampepfl/dotty/issues/4030).
More test cases appear in [#3999](https://github.com/lampepfl/dotty/pull/3999).
We have also made error messages more informative [#3990](https://github.com/lampepfl/dotty/pull/3990).
Fixes to covariant GADTs ([#3989](https://github.com/lampepfl/dotty/issues/3989)/
[#4013](https://github.com/lampepfl/dotty/pull/4013)) have been deferred to next release.

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

