---
layout: blog-page
title: Announcing Dotty 0.6.0 and 0.7.0-RC1
author: Allan Renucci
authorImg: images/allan.jpg
date: 2018-03-05
---

Today, we are excited to release Dotty versions 0.6.0 and 0.7.0-RC1. These releases
serve as a technology preview that demonstrates new language features and the compiler supporting them.

If you’re not familiar with Dotty, it's a platform to try out new language concepts and compiler
technologies for Scala. The focus is mainly on simplification. We remove extraneous syntax
(e.g. no XML literals), and try to boil down Scala’s types into a smaller set of more fundamental
constructs. The theory behind these constructs is researched in
[DOT](https://infoscience.epfl.ch/record/215280), a calculus for dependent object types.
You can learn more about Dotty on our [website](https://nightly.scala-lang.org).

<!--more-->

This is our seventh scheduled release according to our [6-week release schedule](https://nightly.scala-lang.org/docs/usage/version-numbers.html).
The [previous technology preview](https://github.com/scala/scala3/releases/tag/0.6.0-RC1) focussed
on bug fixes and stability work.

## What’s new in the 0.7.0-RC1 technology preview?

### Enum Simplification [#4003](https://github.com/scala/scala3/pull/4003)
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


For more information about [Enumerations](https://nightly.scala-lang.org/docs/reference/enums/enums.html)
and how to use them to model [Algebraic Data Types](https://nightly.scala-lang.org/docs/reference/enums/adts.html),
visit the respective sections in our documentation.


### Erased terms [#3342](https://github.com/scala/scala3/pull/3342)
The `erased` modifier can be used on parameters, `val` and `def` to enforce that no reference to
those terms is ever used. As they are never used, they can safely be removed during compilation.

One particular use case is to add implicit type constraints that are only relevant at compilation
time. For example, let's consider the following implementation of `flatten`.

```scala
class List[X] {
  def flatten[Y](implicit erased ev: X <:< List[Y]): List[Y] = {
    val buffer = new mutable.ListBuffer[Y]
    this.foreach(e => buffer ++= e.asInstanceOf[List[Y]])
    buffer.toList
  }
}

List(List(1, 2), List(3)).flatten // List(1, 2, 3)
List(1, 2, 3).flatten             // error: Cannot prove that Int <:< List[Y]
```

The implicit evidence `ev` is only used to constrain the type parameter `X` of `List` such that we
can safely cast from `X` to `List[_]`. The usage of the `erased` modifier ensures that the evidence
is not used and can be safely removed at compilation time.

For more information, visit the [Erased Terms](https://nightly.scala-lang.org/docs/reference/metaprogramming/erased-terms.html)
section of our documentation.

**Note**: Erased terms replace _phantom types_: they have similar semantics, but with the added
advantage that any type can be an erased parameter. See [#3410](https://github.com/scala/scala3/pull/3410).


### Improved IDE support [#3960](https://github.com/scala/scala3/pull/3960)
The Dotty language server now supports context sensitive IDE completions. Completions now include
local and imported definitions. Members completions take possible implicit conversions into account.

<!-- A GIF would be nice? -->

We also improved the `find references` functionality. It is more robust and much faster!

Try it out in [Visual Studio Code](https://docs.scala-lang.org/scala3/getting-started.html#using-vscode-with-metals)!

### Better and safer types in pattern matching (improved GADT support)

Consider the following implementation of an evaluator for a very simple
language containing only integer literals (`Lit`) and pairs (`Pair`):
```scala
sealed trait Exp
case class Lit(value: Int) extends Exp
case class Pair(fst: Exp, snd: Exp) extends Exp

object Evaluator {
  def eval(e: Exp): Any = e match {
    case Lit(x) =>
      x
    case Pair(a, b) =>
      (eval(a), eval(b))
  }

  eval(Lit(1))                             // 1: Any
  eval(Pair(Pair(Lit(1), Lit(2)), Lit(3))) // ((1, 2), 3) : Any
}
```

This code is correct but it's not very type-safe since `eval` returns a value
of type `Any`, we can do better by adding a type parameter to `Exp` that
represents the result type of evaluating the expression:

```scala
sealed trait Exp[T]
case class Lit(value: Int) extends Exp[Int]
case class Pair[A, B](fst: Exp[A], snd: Exp[B]) extends Exp[(A, B)]

object Evaluator {
  def eval[T](e: Exp[T]): T = e match {
    case Lit(x) =>
      // In this case, T = Int
      x
    case Pair(a, b) =>
      // In this case, T = (A, B) where A is the type of a and B is the type of b
      (eval(a), eval(b))
  }

  eval(Lit(1))                             // 1: Int
  eval(Pair(Pair(Lit(1), Lit(2)), Lit(3))) // ((1, 2), 3) : ((Int, Int), Int)
}
```

Now the expression `Pair(Pair(Lit(1), Lit(2)), Lit(3)))` has type `Exp[((Int,
Int), Int)]` and calling `eval` on it will return a value of type `((Int,
Int), Int)` instead of `Any`.

Something subtle is going on in the definition of `eval` here: its result type
is `T` which is a type parameter that could be instantiated to anything, and
yet in the `Lit` case we are able to return a value of type `Int`, and in the
`Pair` case a value of a tuple type. In each case the typechecker has been able
to constrain the type of `T` through unification (e.g. if `e` matches `Lit(x)`
then `Lit` is a subtype of `Exp[T]`, so `T` must be equal to `Int`). This is
usually referred to as **GADT support** in Scala since it closely mirrors the
behavior of [Generalized Algebraic Data
Types](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type) in
Haskell and other languages.

GADTs have been a part of Scala for a long time, but in Dotty 0.7.0-RC1 we
significantly improved their implementation to catch more issues at
compile-time. For example, writing `(eval(a), eval(a))` instead of `(eval(a),
eval(b))` in the example above should be an error, but it was not caught by
Scala 2 or previous versions of Dotty, whereas we now get a type mismatch error
as expected. More work remains to be done to fix the remaining [GADT-related
issues](https://github.com/scala/scala3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+gadt),
but so far no show-stopper has been found.

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
the IDE sections of the [getting-started page](https://docs.scala-lang.org/scala3/getting-started.html).


### Standalone installation
Releases are available for download on the _Releases_
section of the Dotty repository:
[https://github.com/scala/scala3/releases](https://github.com/scala/scala3/releases)

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
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing
Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.6.0..0.7.0-RC1` these are:

```
   182  Martin Odersky
    94  Nicolas Stucki
    48  Olivier Blanvillain
    38  liu fengyun
    16  Allan Renucci
    15  Guillaume Martres
    11  Aggelos Biboudis
     5  Abel Nieto
     5  Paolo G. Giarrusso
     4  Fengyun Liu
     2  Georg Schmid
     1  Jonathan Skowera
     1  Fedor Shiriaev
     1  Alexander Slesarenko
     1  benkobalog
     1  Jimin Hsieh
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://nightly.scala-lang.org/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/scala/scala3/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
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

