---
layout: blog-page
title: Announcing Dotty 0.25.0-RC2 - speed-up of givens and change in the tuple API
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2020-06-22
---

Hello! We are excited to announce 0.25.0-RC2 of Dotty. In this version, following feedback of the community, we have improved compilation speeds when programming with givens. We have also made some improvements to the tuple API.

You can try out this version right now, from the comfort of your SBT, by visiting the [home page](https://nightly.scala-lang.org/) and scrolling down to the "Create a Dotty Project" section.

Alternatively, you can try this version of Scala online via [Scastie](https://scastie.scala-lang.org/). Once you're there, click "Build Settings" and set "Target" to "Dotty".

Enjoy the ride🚀!

<!--more-->
# Dedicated type for empty tuples
We have added a type `EmptyTuple` to represent empty tuples. Previously empty tuples were represented by `Unit`. This change was done so that all the tuples are a subtype of `Product`. Now, tuples have the following type hierarchy:

```scala
Product -- Tuple -+- EmptyTuple
                  |
                  +- NonEmptyTuple -- *:[Head, Tail <: Tuple]
```

This change impacts existing type class derivation codebases in that they now should use `EmptyTuple` instead of `Unit` in the tuple context.

# Avoid excessive slowdowns when suggesting missing imports in error messages
Dotty brings to the user an enhanced error reporting when it comes to programming with givens. This better error reporting, however, proved to be a trade-off. Sometimes it takes an unreasonable amount of time and space to compute a quality suggestion to the end user.

A number of users reported considerable slowdowns when it comes to programming with implicits. To address this issue, we have modified the logic for given imports suggestion. We introduced a per-run budget of 10 seconds, so it is guaranteed that it won't take longer than that time to compute all the given imports.

This default budget is configurable via a compiler flag `-Ximport-suggestion-timeout`.

This change should speed up the compiler when it comes to programming with givens.

For more information, see PR [#9167](https://github.com/scala/scala3/pull/9167).

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing
Thank you to all the contributors who made this release possible 🎉

According to `git shortlog -sn --no-merges 0.24.0-RC1..0.25.0-RC2` these are:

```
   190  Nicolas Stucki
   175  Martin Odersky
    67  Liu Fengyun
    32  Guillaume Martres
    17  Robert Stoll
    17  bishabosha
     9  Anatolii Kmetiuk
     7  yu-croco
     6  Reto Hablützel
     5  Akhtiam Sakaev
     5  odersky
     4  Raphael Jolly
     4  Ruslan Shevchenko
     4  Olivier Blanvillain
     3  Jamie Thompson
     3  Chris Birchall
     2  Radosław Waśko
     2  Aleksander Boruch-Gruszecki
     2  Eric Loots
     2  Jens Kat
     2  Miles Sabin
     2  noti0na1
     1  Krzysztof Bochenek
     1  Seth Tisue
     1  Tobias Kahlert
     1  Yilin Wei
     1  ansvonwa
     1  FabioPinheiro
     1  december32
     1  yytyd
     1  Ara Adkins
     1  squid314
     1  typeness
     1  xuwei-k
     1  Alex Zolotko
     1  Julien Richard-Foy
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
