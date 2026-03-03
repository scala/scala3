---
layout: blog-page
title: Announcing Dotty 0.22.0-RC1 - syntactic enhancements, type-level arithmetic and more
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2020-02-05
---

Hello! We are excited to announce 0.22.0-RC1 of Dotty. This version brings syntactic enhancements for extension methods and context parameters, as well as the kind projector syntax. Other notable changes include type-level arithmetic, changes to the `inline` parameters semantics and suggestions on missing context parameters.

You can try out this version right now, from the comfort of your SBT, by visiting the [home page](https://nightly.scala-lang.org/) and scrolling down to the "Create a Dotty Project" section.

Alternatively, you can try this version of Scala online via [Scastie](https://scastie.scala-lang.org/). Once you're there, click "Build Settings" and set "Target" to "Dotty".

Enjoy the ride🚀!

<!--more-->
# New syntax for collective extension methods
Extension methods have been present in Dotty for a while. They present an idiomatic way to extend types with methods after these types are defined. For example:

```scala
def (x: Int) toPower (n: Int): Int =
  @annotation.tailrec def loop(accum: Int, power: Int): Int =
    if power == 0 then accum
    else if power > 0 then loop(accum * x, power - 1)
    else throw IllegalArgumentException("This operation only supports positive powers")
  loop(1, n)
  println(s"3^3 = ${3.toPower(3)}")  // 3^3 = 27
```

However, when one wants to define multiple extension methods for a type, a lot of boilerplate manifests:

```scala
def (x: Int) toPower (n: Int): Int = ???
def (x: Int) squared = ???
def (x: Int) asBinaryString = ???
```

The type in question and the name of its parameter, `(x: Int)`, repeat.

This boilerplate was the motivation to introduce collective extension methods. For a while, we were experimenting with looking at these through the lens of the `given` mechanism. We have tried out an idea of making these methods belong to an object visible in the `given` scope and, if such an object is present in the `given` scope, its extension methods are also automatically usable.

However, `given` instances are about *types* and the collective extension methods describe *parameters* of extension methods. Hence, in this release we introduce a new syntax for the collective extension methods:

```scala
extension listOps on [T](xs: List[T]) {
  def second = xs.tail.head
  def third: T = xs.tail.tail.head
}

val list = List(1, 2, 3)
println(s"Second: ${list.second}")  // 2
println(s"Third: ${list.third}")  // 3
```

This syntax is a completely separate one from the `given` syntax and hence is aimed to bring more clarity and disentangle the two different concepts.

For the discussion, see [PR #7917](https://github.com/scala/scala3/pull/7917). For more information on how to use extension methods in general and collective extension methods in particular, see the [documentation](https://nightly.scala-lang.org/docs/reference/contextual/extension-methods.html).

# Kind projector syntax support
[Kind projector](https://github.com/typelevel/kind-projector) is a popular compiler plugin for Scala 2. It is especially useful in the context of purely functional programming and type class derivation – everywhere where you need to work extensively with types.

As of this release, a subset of the kind projector syntax is now supported in Dotty. Credits for this contribution go to [Travis Brown](https://github.com/travisbrown).

To enable it, you need to run the compiler with the `-Ykind-projector` flag. You can e.g. write the following:

```scala
// Fix #7139: Implement kind-projector compatibility #7775
// With -Ykind-projector

trait Functor[F[_]]
  def map[A, B](fa: F[A], f: A => B): F[B]

object eitherFunctor extends Functor[Either[Int, *]]
  def map[A, B](fa: Either[Int, A], f: A => B): Either[Int, B] = fa match
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)

object functionFunctor extends Functor[Int => *]
  def map[A, B](fa: Int => A, f: A => B): Int => B =
    fa andThen f

object tupleFunctor extends Functor[λ[x => (x, x)]]
  def map[A, B](fa: (A, A), f: A => B): (B, B) = fa match
    case (a1, a2) => (f(a1), f(a2))

@main def Test =
  val tpl = (1, 2)
  val squared = tupleFunctor.map(tpl, a => a * a)
  println(squared)  // (1,4)
```

For the discussion, see [PR #7775](https://github.com/scala/scala3/pull/7775). Also see the GitHub [repository](https://github.com/typelevel/kind-projector) of the kind projector Scala 2 plugin for more context.

# Further improvements to the context parameters syntax
Scala 3 context parameters are successors of Scala 2 implicits. In Scala 2, they proved useful for a wide range of applications including purely functional programming, dependency injection, type class derivation, type-level programming. Because their apparent value, one of the priorities in Scala 3 for us is to improve the conceptual framework behind them.

The state of context parameters before this release heavily employed the `given` keyword. For example:

```scala
// OLD SYNTAX BELOW
given String = "10"
given (given str: String) : Int = str.toInt
def f(x: Int)(given y: Int) = x * y
```

The above is a suboptimal solution, however. The feedback we received from the community suggested that many people felt like the `given` keyword was overused, similarly to the `implict` keyword in Scala 2. This overuse is one of the things we'd like to avoid in Scala 3. It leads, for example, to situations like `given (given ...)` which are not nice to read.

For this release, we have changed the syntax for the context parameters. The keyword for the context argument group is now `using` instead of `given`. The above snippet now becomes:

```scala
given String = "10"
given (using str: String) as Int = str.toInt
def f(x: Int)(using y: Int) = x * y
```

On the call site, the syntax for explicitly specifying the context parameters is now:

```scala
f(2)(using 20)
```

As opposed to the previous:

```scala
// OLD SYNTAX BELOW
f(2)(given 20)
```

For the time being, the change is experimental and the old syntax is also supported. For the discussion, see [PR #8162](https://github.com/scala/scala3/pull/8162). You can browse the documentation concerning the new syntax [here](https://nightly.scala-lang.org/docs/reference/contextual/motivation-new.html).

# Semantics of inline parameters changed
Inline parameters is a metaprogramming feature of Dotty which allows to splice the body of the parameter on its call site. Previously, inline parameters to methods were required to be known on compile time. With this release, this constraint has been relaxed. The following:

```scala
inline def sumTwice(a: Int, b: =>Int, inline c: Int) = a + a + b + b + c + c
sumTwice(f(), g(), h())
```

Translates to:

```scala
    val a = f()
    def b = g()
    a + a + b + b + h() + h()
```

Notice how the value of the by-name parameter `b` is not inlined but is bound to `def b`. This is an important change that affects all the macros that accepted by-name parameters and analyzed the AST of the underlying code. With this release, such macros will stop working correctly because the AST of the code in question will be the identifier of the by-name parameter, `b` in this case, and not the AST of the code passed under that parameter's name. The workaround is to change all the by-name parameters in your macros to inline parameters.

So, if previously you had a macro `inline def operationOnCode(code: => Unit) = ${ mcrImpl('code) }` which did something on the AST of the passed `code`, with this release you need to change it to `inline def operationOnCode(inline code: Unit) = ${ mcrImpl('code) }`.

This change was introduced by [PR #8060](https://github.com/scala/scala3/pull/8060/).

Another change in the semantics of the inline parameters involves the fact that the can no longer be passed as constants to macro implementations. Previously, the following was possible:

```scala
// OLD SEMANTICS
inline def power(x: Double, inline n: Int) = ${ powerCode('x, n) }
private def powerCode(x: Expr[Double], n: Int)(given
QuoteContext): Expr[Double] = ???
```

It was possible to pass `n` directly to the spliced `powerCode` and it would have been treated as a constant in that macro implementation.

Now, the inline parameters must be quoted when passed to a macro:

```scala
inline def power(x: Double, inline n: Int) = ${ powerCode('x, 'n) }
private def powerCode(x: Expr[Double], n: Expr[Int])(given QuoteContext): Expr[Double] = ???
```

You can obtain the constant value of `n` from within the macro implementation by calling `n.getValue` on it which returns an `Option`. This change was introduced by [PR #8061](https://github.com/scala/scala3/pull/8061).

For more information about the inline capability of Dotty, see [documentation](https://nightly.scala-lang.org/docs/reference/metaprogramming/inline.html).

# Primitive compiletime operations on singleton types
Contributed by [Maxime Kjaer](https://github.com/MaximeKjaer), this release brings along type-level arithmetic:

```scala
import scala.compiletime.ops.int._

val x: 2 + 3 = 5  // OK
val y: 3 * 4 + 1 = 12  // error
```

The compile-time error above will say:

```scala
4 |val y: 3 * 4 + 1 = 12
  |                   ^^
  |                   Found:    (12 : Int)
  |                   Required: (13 : Int)
```

This feature is particularly useful for data science applications. In data science, it is very easy to make a linear algebra mistake, multiply matrices of wrong dimensions and get a runtime error – sometimes after a few hours of running the model. Hence compile-time verification of the models has a great potential for saving time. With such a type-level arithmetic, Scala becomes well-positioned to implement such type-safe data science frameworks.

For the discussion, see [PR #7628](https://github.com/scala/scala3/pull/7628). The documentation is available [here](https://nightly.scala-lang.org/docs/reference/metaprogramming/inline.html#the-scalacompiletimeops-package).

# Suggestions on missing context parameters
If there's a compile-time error due to a missing context parameter and this error can be fixed with an import, the compiler will attempt to suggest such an import in the error message. Here is an example of how this error looks like:

```
-- Error: tests/neg/missing-implicit1.scala:17:4 -----------------------------------------------------------------------
17 |  ff // error
   |    ^
   |no implicit argument of type testObjectInstance.Zip[Option] was found for parameter xs of method ff in object testObjectInstance
   |
   |The following import might fix the problem:
   |
   |  import testObjectInstance.instances.zipOption
```

One area where these suggestions will make life easier is purely functional programming with type-classes, with libraries like [cats](https://typelevel.org/cats/). Having the fix for a missing type class in the error message itself is a big time-saver.

For the discussion, see [PR #7862](https://github.com/scala/scala3/pull/7862).

# TASTy Inspector library
TASTy Consumer was renamed to TASTy Inspector as of this release. It was also published in a library of its own. For more information, see the [documentation](https://nightly.scala-lang.org/docs/reference/metaprogramming/tasty-inspect.html) on this library.

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.21.0-RC1..0.22.0-RC1` these are:

```
   192  Martin Odersky
    85  Nicolas Stucki
    57  Antoine Brunner
    42  Liu Fengyun
    29  Guillaume Martres
    23  Aggelos Biboudis
    17  Maxime Kjaer
    10  Anatolii
     7  Jamie Thompson
     4  Minghao Liu
     3  Travis Brown
     3  Andrew Valencik
     2  fhackett
     2  Dvir Faivel
     2  Nadezhda Balashova
     2  Ruslan Shevchenko
     2  Lan, Jian
     2  Anatolii Kmetiuk
     2  Yevgen Nerush
     1  Dale Wijnand
     1  odersky
     1  Dmitrii Naumenko
     1  Eric K Richardson
     1  Eric Loots
     1  Jaap van der Plas
     1  Keith Pinson
     1  Miles Sabin
     1  Alexander Shamukov
     1  Som Snytt
     1  Taisuke Oe
     1  Timothée Floure
     1  bishabosha
     1  gzoller
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://nightly.scala-lang.org/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/scala/scala3/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
They make perfect entry points into hacking on the compiler.

We are looking forward to having you join the team of contributors.

## Library authors: Join our community build

Dotty now has a set of widely-used community libraries that are built against every nightly Dotty
snapshot. Currently, this includes ScalaPB, algebra, scalatest, scopt and squants.
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
