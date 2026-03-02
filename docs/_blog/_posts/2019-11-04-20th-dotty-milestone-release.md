---
layout: blog-page
title: Announcing Dotty 0.20.0-RC1 – `with` starting indentation blocks, inline given specializations and more
author: Anatolii Kmetiuk
authorImg: images/anatolii.png
date: 2019-11-04
---

Greetings! We are excited to announce the 20th release of Dotty. This release brings a bunch of improvements to the language, such as `with` keyword starting an indentation block, normal parameters after given parameters, inline givens specialization and more.

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

# What’s new in the 0.20.0-RC1 technology preview?
## Syntax change for type parameters of extension methods
When writing extension methods with type parameters, the type parameters must come first, e.g.:

```scala
def [T](xs: List[T]) append (ys: List[T]): List[T] = ...
```

Previously, the same would have been written as:

```scala
def (xs: List[T]) append [T] (ys: List[T]): List[T] = ...
```

An argument for the old syntax is that it aligns the definition and call syntax. On the other hand, the new syntax maintains the general rule that parameter introductions always come before parameter uses. The decisive argument to switch is to be consistent with the new collective parameter syntax, where `append` would be written like this:

```scala
given [T](xs: List[T])
  def append (ys: List[T]): List[T] = ...
```

To avoid misalignment of type parameters between definition and call syntax, we considered disallowing explicit type parameters for extension methods altogether, and to require that the method is called as a normal method instead. But that would not work for anonymous givens as in the last example above.

## Infer `private[this]`
We now infer the `private[this]` modifier for variables if all the accesses to a variable are via this. Explicit `private[this]` and `protected[this]` in code are deprecated under the `-strict` flag.

The main reasons for dropping `private[this]` are:

- It is syntactically an irregular case. A pair of brackets usually encloses a type, but `this` is a value.
- Its effect over `private` is purely local and can be easily inferred.
- It leads to bike shedding: should I use `private` or `private[this]`? One is shorter but the other might be more efficient.

`protected[this]` by now influences compiler decisions in no way at all. Hence it is reasonable to drop it.

## `with` keyword's new role
`with` keyword can now optionally precede the class body. So that you can write your classes as follows:

```scala
trait A with {
  def f: Int
}
class C(x: Int) extends A with {
  def f = x
}
type T = A with {
  def f: Int
}
```

Or, equivalently:

```scala
trait A with
  def f: Int
class C(x: Int) extends A with
  def f = x
type T = A with
  def f: Int
```

The problem this change solves is that it is very easy to accidentally outdent a class member – and it will end up outside the class. The benefit of the new `with` is that starts an indentation block. Since the compiler knows for sure an indentation block must follow, it will emit an error if you forget to indent your statement.

## Inline `given` specialization
It is now possible to specialize `inline given`s with the help of `<:` as follows:

```scala
trait A
class B extends A

inline given tc <: A = B()

val x: B = summon[A]
```

This change brings `given`s even with the ordinary `inline def`s.

## Normal parameters can follow `given` parameters
Previously normal parameters after `given` parameter was disallowed mainly because they looked awkward with the old syntax. With the syntax being improved, this restriction is now lifted and you can write, e.g., the following program:

```scala
class C(val x: Int)
def f(x: Int)(given c: C)(y: Int) = x + c.x + y
```

## `then` is optional at line end
So the following is now legal:

```scala
val y1 =
  if x > 0
    1
  else
    2
```

It is easy to forget to put `then` at the end of the line if nothing else follows it, but also easy to infer that it must be inserted there.

## Metaprogramming Progress
We are making a steady progress developing and improving the metaprogramming features of Dotty. Here are metaprogramming highlights of this release:

- Fix #7189: Do not try to load contents if file does not exist [#7476](https://github.com/scala/scala3/pull/7476)
- Add customizable names for definitions in quotes [#7346](https://github.com/scala/scala3/pull/7346)
- Rename scala.quoted.matching.{Bind => Sym} [#7332](https://github.com/scala/scala3/pull/7332)
- Replace AsFunction implicit class with Expr.reduce [#7299](https://github.com/scala/scala3/pull/7299)

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.19.0-RC1..0.20.0-RC1` these are:

```
    99  Martin Odersky
    64  Nicolas Stucki
    16  Nikita Eshkeev
    15  Guillaume Martres
     9  Robert Stoll
     8  Anatolii
     5  Liu Fengyun
     5  Olivier Blanvillain
     3  Miles Sabin
     2  Aggelos Biboudis
     2  Jamie Thompson
     2  Antoine Brunner
     2  Ben Elliott
     2  Guillaume R
     1  noti0na1
     1  Ashwin Bhaskar
     1  Batanick
     1  Bojan Dunaj
     1  Harpreet Singh
     1  Lucas
     1  Lucas Jenß
     1  Martijn Hoekstra
     1  bishabosha
     1  brunnerant
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
