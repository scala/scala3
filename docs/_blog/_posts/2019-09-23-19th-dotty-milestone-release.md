---
layout: blog-page
title: Announcing Dotty 0.19.0-RC1 – further refinements of the syntax and the migration to 2.13.1 standard library
author: Anatolii Kmetiuk
authorImg: images/anatolii.png
date: 2019-09-23
---

Greetings! With this post, we are proud to announce the 19th release of Dotty. This release features further changes to the syntax following the feedback from the community and further discussion. Another important change is the migration to the 2.13.1 standard library.

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

You can learn more about Dotty on our [website](https://dotty.epfl.ch).

<!--more-->

# What’s new in the 0.19.0-RC1 technology preview?
## Given syntax reworked
`the` method (a better version of `implicitly` in Scala 3) was renamed to `summon`.

`given` definitions now closely resemble ordinary definitions:

```scala
given Int = 10  // Anonymous
given x: String = "foo"  // Named
given f(given x: Int): Option[Int] = Some(x * x)  // With given parameters
given [T](given opt: Option[T]): List[T] = opt.toList  // Anonymous with type parameters

@main def Test = println(summon[List[Int]])
```

Note that `as` was dropped and `given` must now go inside the parentheses as opposed of being used in an infix style.

All of the experimental syntax related to givens – such as `delegate for`, `given as`, infix-style `given` – is now dropped.

## Colons dropped from class or object definitions
Now you can define an object as follows:

```scala
object Bar
  val x = 10

@main def Test = println(Bar.x)  // 10
```

In `0.18.1-RC1`, you would have needed to put a colon after `Bar`. The colon was also dropped for traits, classes and enums.

## Allow `given` in pattern bindings
Consider you have the following monadic flow:

```scala
for
  x   <- bar
  res <- foo(given x)
yield res
```

Writing entire programs in a monadic flow is not uncommon in functional programming. When working in this style, a situation may arise as shown above: one statement of the monadic flow implicitly depends on the result of another one. It was impossible to declare a pattern variable as a given, which necessitated passing it around explicitly. Not anymore! Now, you can write the above code as follows:

```scala
for
  given x: Int <- bar  // Int, or whatever type you are extracting
  res <- foo
yield res
```

Note that the type of the given variable must be specified explicitly.

Full example:

```scala
def foo(given x: Int): Option[Int] = Some(x * x)
def bar = Some(10)

@main def Test =
  for
    given x: Int <- bar
    res <- foo
  yield println(res)
```

This syntax is allowed anywhere where a pattern is allowed. So you can write:

```scala
user match
  case User(_, Some(given email: Email)) => sendEmail
```

Full example:

```scala
opaque type Email = String
object Email
  def apply(value: String): Email = value

def sendEmail(given m: Email): Unit =
  println(s"Sent an email to $m")

case class User(name: String, email: Option[Email])

@main def Test =
  val user = User("Tom", Some(Email("tom@gmail.com")))
  user match
    case User(_, Some(given email: Email)) => sendEmail
```

## Replace given matches by a library method
Given matches was a feature that allowed to query the implicit scope and execute different logic based on what was found there. We have replaced this feature with a library method called `summonFrom`. You can use it as follows:

```scala
import compiletime.summonFrom

given Int = 10

@main inline def Test = summonFrom {
  case str: String => println(s"String $str")
  case int: Int => println(s"Int $int")  // Int 10
}
```

The above code will print "Int 10" since an integer with the value 10 was present in the implicit scope but no String was present.

Notice that we had to define the `Test` method as `inline` since `summonFrom` can only be used from an inline method.

## Wildcard types written with `?`
You can now use both `_` and `?` to express wildcard types. For example:

```scala
@main def Test =
  val xs: List[Int] = (1 to 10).toList
  xs match
    case xss: List[?] => println(s"It is a list")
```

This is the first step in a multi-step process to disallow `_` as wildcards so that we can use underscores for both terms and type parameters instead. This will make the language more regular.

## Lambda parameters must be enclosed in parentheses
Lambda parameters with type ascriptions are now required to be enclosed in parentheses. E.g. `x: Int => x * x` is no longer legal, it must be written as `(x: Int) => x * x`. However, you can still write `x => x * x`, that is, if `x` does not have an explicit type ascription.

## Dottydoc redesign
The output of [Dottydoc](https://dotty.epfl.ch/docs/usage/dottydoc.html) has been redesigned. It is now fully responsive: every element, including API docs and search, is adapted to both small and big screens.

The most visible changes are the toolbar and the sidebar. They now have a common darker background, which makes them more readable and helps separating navigation from content. Also, the sidebar is collapsible and has been optimized so that it doesn't glitch when the page loads.

The toolbar's logo can now be set with the `-project-logo` option.
For instance, `-project-logo dotty-logo.svg` will make `/images/dotty-logo.svg` appear in the toolbar.

[The front page](https://dotty.epfl.ch) has been redesigned too, with a new responsive menu and improved contrast.

Overall, every page has been updated with consistent settings of fonts and colors. A more detailed comparison between the new and the old design can be found [here](https://github.com/lampepfl/dotty/pull/7153).

## Metaprogramming Progress
We're making steady progress on the Dotty metaprogramming capability. In our previous work, we've implemented a bunch of functions for working with expressions. For example, we have a capability to convert a list of expressions into an expression of list, or a tuple of expressions into an expression of tuple.

In this release, we have collected this family of functions in one place – the companion object `scala.quoted.Expr`. Currently, the following methods are available in that object for working with expressions:

- nullExpr – an expression of `null`.
- unitExpr – an expression of `unit`.
- block – given a list of statements and a final expression, concatenates them in a block.
- ofSeq, ofList – constructs an expression of collection from a collection of expressions
- ofTuple – constructs an expression of tuple from either a tuple of expressions or a sequence of expressions.

Also, `x.toExpr` syntax which lifts `x` into an expression is now deprecated. It is replaced with `Expr(x)`.

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.18.1-RC1..0.19.0-RC1` these are:

```
    87  Martin Odersky
    50  Nicolas Stucki
    42  Guillaume R
    33  Nikita Eshkeev
    20  Guillaume Martres
     9  Liu Fengyun
     8  Anatolii
     5  Robert Stoll
     3  Miles Sabin
     1  Sam Desborough
     1  Anatolii Kmetiuk
     1  Jon Pretty
     1  Oron Port
     1  Aggelos Biboudis
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://dotty.epfl.ch/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/lampepfl/dotty/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
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
