---
layout: blog-page
title: Announcing Dotty 0.18.1-RC1 – switch to the 2.13 standard library, indentation-based syntax and other experiments
author: Anatolii Kmetiuk
authorImg: images/anatolii.png
date: 2019-08-30
---

Greetings! With this post, we are proud to announce the 18th release of Dotty. With this release, we have switched to the 2.13 standard library (which is why the patch version of Dotty is now `1`)🎉. We are also conducting more experiments with the language syntax which will hopefully result in a better, cleaner way to write Scala programs.

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

This is our 18th scheduled release according to our
[6-week release schedule](https://nightly.scala-lang.org/docs/contributing/procedures/release.html).

# What’s new in the 0.18.1-RC1 technology preview?
The hottest change of this release is a series of experiments with the language syntax. Some of them are controversial, some of them are almost unanimously considered useful. Regardless, the underlying motivation for all of them is something all of us want, we believe d: to make programming in Scala easier to write and read, drop unnecessary boilerplate, facilitate idiomatic programming.

We are thrilled to have this unique opportunity to experiment while Scala 3 is still in its inception and malleable. This is the only time we can try out significant language changes, and we are determined to make the most out of it.

Our view on these trials is that, like with any big change, we need time to see if these are good ideas. We don't know if they will work or not. We believe that the only way to find out is to play with them for some time.

Some of these changes will end up in Scala 3, some of them will be deemed not worth it. One way or another, trying the new look of an old language is an educational and fun experience.

Keeping that in mind, let us proceed to the nitty-gritty!

## Switch to Standard Library 2.13
Dotty is now using the standard library 2.13 instead of the previous 2.12.8.

## `@main` functions
Bootstrapping a new Scala application is as hard as a new Java application. How do you write a main method? Normally, something like that:

```scala
object Test {
  def main(args: Array[String]): Unit = println(s"Hello World")
}
```

You need to define at least two things that serve no real purpose: an object and `(args: Array[String])`.

Not anymore! Meet the `@main` functions:


```scala
@main def test: Unit = println(s"Hello World")
```

The above generates the following code at the top-level of the compilation unit (source file):

```scala
final class test {
  <static> def main(args: Array[String]): Unit =
    try Main$package.test() catch
      {
        case
          error @ _:scala.util.CommandLineParser.CommandLineParser$ParseError
         => scala.util.CommandLineParser.showError(error)
      }
}
```

So, a `final class` is generated with the same name as the `@main` method and the `def main(args: Array[String])` inside. The body of this method calls the original `test()` method. Since it is a top-level definition, it resides in the synthetic `Main$package` object generated for the `Main.scala` source being compiled.

An astute reader has probably noticed the mentions of things like `CommandLineParser` in the body of the generated method, which hints to certain features. That's right, we support a basic ability for command-line args parsing:

```scala
@main def sayHello(name: String, age: Int): Unit =
  println(s"Hello $name, you are $age years old")
```

If you run the above with command line arguments "Jack 25", the output will be "Hello Jack, you are 25 years old". And here is how you can define a custom parser for your own class:

```scala
case class Address(city: String, street: String)

given scala.util.FromString[Address] {
  /** Can throw java.lang.IllegalArgumentException */
  def fromString(s: String): T =
    s.split(",").toList match {
      case city :: street :: Nil => Address(city, street)
      case _ => throw new IllegalArgumentException(s"Please specify address in the format 'city, street'")
    }
}

@main def sayHello(addr: Address): Unit =
  println(s"You are living at $addr")
```

The motivation for the `@main` functions is to make Scala scripting friendly. So far we do not plan to support something more complex than the above – we believe if a user needs a complex command line parsing capability, they can always fall back to the conventional `def main(args: Array[String])` syntax plus a dedicated library like [scopt](https://github.com/scopt/scopt). The changes described above, however, are already enough to make script development much less tedious than before.

To learn more, see the [documentation](https://nightly.scala-lang.org/docs/reference/changed-features/main-functions.html).

## Allow infix operators at the start of the line
A small change yet relevant to many. Now, you can write the following code:

```scala
def isABorC(x: Char) = x == 'A'
  || x == 'B'
  || x == 'C'
```

Prior to this change, it was only possible to express infix operators at the beginning of the line as follows:

```scala
def isABorC(x: Char) = (x == 'A'
  || x == 'B'
  || x == 'C')
```

## Drop do-while syntax
Remember that obscure `do-while` feature of Scala 2 where you could write:

```scala
scala> var x = 0
x: Int = 0

scala> val iterator = Iterator.from(10, -1)
iterator: Iterator[Int] = <iterator>

scala> do {
     |   x = iterator.next()
     |   println(x)
     | } while (x > 0)
10
9
8
7
6
5
4
3
2
1
0
```

Well, it is no more! That is the only place where the `do` token is used in Scala, the feature itself is rarely employed, and it would be nice to reclaim the `do` token for other uses (described in details in the section on the new syntax for control expressions).

The language does not lose its expressiveness though – you can still write the following to achieve the same functionality:

```scala
val iterator = Iterator.from(10, -1)

@main def test = {
  var x: Int = 0
  while ({
    x = iterator.next
    println(x)
    x > 0
  }) ()
}
```

For more information, see PR [#6994](https://github.com/scala/scala3/pull/6994).

## Brace-less syntax for control expressions
This is an effort to clean-up the control expressions. Scala 2 has two ways of writing `if` statements – with and without parentheses. Parentheses can be dropped in Scala 2 `if`s inside `match` or `for` statements. We'd like to have a single style of writing all of the control expressions, and the cleaner the better.

This release, hence, brings the ability to write all of the control expressions without braces. E.g.:

```scala
@main def testFor = {
  val xs = 0 to 10
  val xsFiltered = for x <- xs if x > 1 yield x
  for
    x <- xsFiltered
    y <- xsFiltered
  do println(s"$x * $y = ${x * y}")
}

@main def testIf(day: String) = {
  if day == "Sunday" || day == "Saturday" then println("Today is a weekend, hooray!")
  else println(s"Today is a workday.")
}

@main def testWhile(bound: Int) = {
  var x = 0
  def incrementX() = {
    x += 1
    println(x)
  }
  while x <= bound do incrementX()
}
```

Moreover, the compiler can automatically rewrite your sources from the old syntax to the new syntax and vice versa. To rewrite the sources to the new syntax, run the compiler with the `-rewrite -new-syntax` flags, and to rewrite to the old syntax, use `-rewrite -old-syntax`. So far, both syntaxes are supported.

For more information and the precise rules, see PR [#7024](https://github.com/scala/scala3/pull/7024).

## Significant indentation syntax
Significant indentations syntax is here! A logical continuation of the brace-less syntax for control expressions described above, meant as an exploration into a better way to write Scala, it allows writing Scala programs without braces. For example:

```scala
enum Day:
  case Monday, Tuesday, Wednesdey, Thursday, Friday, Saturday, Sunday
  def isWeekend: Boolean = this match
    case Saturday | Sunday => true
    case _ => false

given as scala.util.FromString[Day]:
  def fromString(str: String): Day =
    try Day.valueOf(str)
    catch
      case _: IllegalArgumentException =>
        throw new IllegalArgumentException(s"$str is not a valid day")

@main def test(day: Day) =
  if day.isWeekend then
    println("Today is a weekend")
    println("I will rest")
  else
    println("Today is a workday")
    println("I will work")
```

So far, it is a purely experimental effort. This means there is no final decision yet on whether or not it will be included in Scala 3. However, we treat this feature seriously enough to give it an extended period of trial and see if it is viable as the new look and feel for Scala.

For more details and the discussion, see PRs [#7083](https://github.com/scala/scala3/pull/7083) and [#7114](https://github.com/scala/scala3/pull/7114).

## Generic Number Literals
It is now possible to seamlessly integrate with different number formats: that is, to write a number and get it automatically converted to your class of choice. E.g.:

```scala
import scala.util.FromDigits

case class Digits(ds: List[Char])

given as FromDigits[Digits] = (digits: String) => Digits(digits.toList)

@main def test =
  val x: Digits = 1234
  println(x)  // Digits(List('1', '2', '3', '4'))
```

If a number is written in place where a non-numeric type is expected and there is an `FromDigits` given in scope, this given will be used to convert the number (presented as `String`) to that type.

For precise rules, semantics and a larger example of `BigFloat`, see [the documentation](https://nightly.scala-lang.org/docs/reference/changed-features/numeric-literals.html).

## Metaprogramming Progress
We are making steady progress with the language metaprogramming features. The metaprogramming spotlights of this release are as follows:

- `toExprOfTuple` method which allows converting a `Seq[Expr[Any]]` to `Expr[Tuple]`. The types of the expressions will be preserved in the tuple. See [#7037](https://github.com/scala/scala3/pull/7037) and [#7076](https://github.com/scala/scala3/pull/7076) for the details.
- `toExprOfTuple` method that converts a tuple of expressions to an expression of tuple – see [#7047](https://github.com/scala/scala3/pull/7047).
- `toExprOfSeq` which converts an `Seq[Expr[A]]` to `Expr[Seq[A]]` – see [#6935](https://github.com/scala/scala3/pull/6935).
- More `Liftable` instances – for Tuples of arity greater than 22, `BigInt` and `BigDecimal` – see [#6947](https://github.com/scala/scala3/pull/6947) and [#6944](https://github.com/scala/scala3/pull/6944).
- Leverage implicit lambdas to simplify `Liftable.toExpr` method – see [#6924](https://github.com/scala/scala3/pull/6924) to learn how it is done.
- Runtime staging `run` moved to `scala.quoted.staging` in [#7077](https://github.com/scala/scala3/pull/7077).
- Runtime staging factored out to a separate library in [#7080](https://github.com/scala/scala3/pull/7080).

## Type Class Derivation
Type class derivation has received a major rework and an [updated documentation](https://nightly.scala-lang.org/docs/reference/contextual/derivation.html). We have dropped the usage of the `Shape` type to describe the shape of a type. Instead, all the relevant information is now encoded in the `Mirror` type and its subtypes as tuples.

For more information, see the [documentation](https://nightly.scala-lang.org/docs/reference/contextual/derivation.html).

## Other
- This release also features the new version of the SBT Dotty Plugin – 0.3.4. It contains some bug fixes – see [#7120](https://github.com/scala/scala3/pull/7120) for details.
- Scala Days 2019 talks related to Dotty are now [mentioned](https://nightly.scala-lang.org/docs/resources/talks.html) at our website – this allows to systematize the knowledge about the next generation of Scala in one place – see [#6984](https://github.com/scala/scala3/pull/6984).
- ScalaJS needs your help! We would like to have robust support for ScalaJS in Dotty, which unfortunately is not the case so far. If you are interested in contributing, please see [the getting started tutorial](https://gist.github.com/sjrd/e0823a5bddbcef43999cdaa032b1220c) and [the discussion](https://github.com/scala/scala3/issues/7113).

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.17.0-RC1..0.18.1-RC1` these are:

```
   106  Nicolas Stucki
    84  Martin Odersky
    68  Guillaume Martres
    26  Liu Fengyun
    24  Jamie Thompson
    23  Miles Sabin
    16  Anatolii
     8  Sébastien Doeraene
     7  bishabosha
     4  Aggelos Biboudis
     4  Michał Gutowski
     2  odersky
     2  Nikolay
     1  Master-Killer
     1  Ashwin Bhaskar
     1  Carlos Quiroz
     1  =
     1  Olivier Blanvillain
     1  SrTobi
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
