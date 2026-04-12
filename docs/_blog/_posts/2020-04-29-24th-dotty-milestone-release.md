---
layout: blog-page
title: Announcing Dotty 0.24.0-RC1 - 2.13.2 standard library, better error messages and more
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2020-04-29
---

Hello! We are excited to announce 0.24.0-RC1 of Dotty. In this version, we have updated the standard library to 2.13.2. Also, we have made some work to make error messages more user-friendly and a bunch of other polishings to the language.

You can try out this version right now, from the comfort of your SBT, by visiting the [home page](https://nightly.scala-lang.org/) and scrolling down to the "Create a Dotty Project" section.

Alternatively, you can try this version of Scala online via [Scastie](https://scastie.scala-lang.org/). Once you're there, click "Build Settings" and set "Target" to "Dotty".

Enjoy the ride🚀!

<!--more-->
# REPL works with indented code
REPL now supports indented code. Consider the following snippet:

```scala
scala> if true then
     |   print(1)
     |   print(2)
     |
```

Previously, the REPL would have stopped after `print(1)`. Now, it waits either for an `else` block or an extra newline to indicate the end of the expression. The above example will output `12` as expected.

# Better error message for ifs that miss an else branch
The error messages are now more beginner-friendly. Consider the following:

```scala
def f: Int = if ??? then 1
```

Above, the `if` expression returns a `Unit` since an `else` clause is missing. Previously, the user would have gotten the following error:

```
-- [E007] Type Mismatch Error: ...
12 |def f: Int = if ??? then 1
   |             ^^^^^^^^^^^^^
   |             Found:    Unit
   |             Required: Int
```

Now, the above error message also contains the following sentence:

```
   |             Maybe you are missing an else part for the conditional?
```

We hope this change will make the language more intuitive for new users.

# Inline overrides
Inline overrides are now supported. For example, consider the following code:

```scala
abstract class A:
  def f(x: Int) = s"Foo $x"

class B extends A:
  inline override def f(x: Int) = s"Bar $x"

@main def Test =
  val b = B()
  println(b.f(22))
  val a: A = b
  println(a.f(22))
```

The output of the above program is:

```
Bar 22
Bar 22
```

This new change, however, comes with rather intricated rules – if you are interested to learn about them in details, see [documentation](https://nightly.scala-lang.org/docs/reference/metaprogramming/inline.html#rules-for-overriding) on inlines and the PR #[8543](https://github.com/scala/scala3/pull/8543/files) which introduced the change.

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing

Thank you to all the contributors who made this release possible 🎉

According to `git shortlog -sn --no-merges 0.23.0-RC1..0.24.0-RC1` these are:

```
   136  Martin Odersky
    74  Nicolas Stucki
    37  Guillaume Martres
    33  Robert Stoll
    22  Liu Fengyun
    19  Anatolii Kmetiuk
    16  Arnaud ESTEVE
    15  Olivier Blanvillain
    10  Arnaud Esteve
     9  Martijn Hoekstra
     6  Anatolii
     4  Som Snytt
     4  bishabosha
     4  Aleksander Boruch-Gruszecki
     3  Miles Sabin
     2  odersky
     2  Fengyun Liu
     2  Julien Richard-Foy
     1  Ara Adkins
     1  Maxime Kjaer
     1  Philippus
     1  Rike-Benjamin Schuppner
     1  Julien Jean Paul Sirocchi
     1  Dani Rey
     1  Sébastien Doeraene
     1  aesteve
     1  Dale Wijnand
     1  fhackett
     1  gzoller
     1  Michael Pilquist
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://nightly.scala-lang.org/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/scala/scala3/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
They make perfect entry points into hacking on the compiler.

We are looking forward to having you join the team of contributors.

## Library authors: Join our community build

Dotty now has a set of widely-used community libraries that are built against every nightly Dotty
snapshot. Currently, this includes shapeless, ScalaPB, algebra, scalatest, scopt and squants.
Join our [community build](https://github.com/scala/scala3/tree/main/community-build)
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
