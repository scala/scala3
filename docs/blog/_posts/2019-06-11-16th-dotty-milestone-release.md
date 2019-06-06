---
layout: blog-page
title: Announcing Dotty 0.16.0-RC1 – the Scala Days 2019 release
author: Aggelos Biboudis
authorImg: /images/aggelos.png
date: 2019-06-11
---

Hello again! Today, we are super excited to announce the 16th release of Dotty.
The development of Dotty continues according to our schedule but today, Tuesday
June the 11th, we are extra excited as it is the first day of [Scala Days 2019](https://scaladays.org/)
which marks the 10th anniversary of Scala Days. With this release we bring
improvements and a few new features getting closer to the envelop of the new
features that Dotty plans to offer.

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

You can learn more about Dotty on our [website](https://dotty.epfl.ch).

<!--more-->

This is our 16th scheduled release according to our
[6-week release schedule](https://dotty.epfl.ch/docs/contributing/release.html).

# What’s new in the 0.16.0-RC1 technology preview?

<!-- https://github.com/lampepfl/dotty/pulls?q=is%3Apr+closed%3A%3E2019-05-23+is%3Aclosed+sort%3Acomments-desc -->

## Polymorphic function types

We add support for _polymorphic function types_. Nowadays if we want to write a
universally quantified function over elements of lists of type `T` we write
e.g., `List[T] => List[(T, T)]`.

```scala
[T <: AnyVal] => List[T] => List[(T, T)]
```

## Other changes
Some of the other notable changes include the following:

- Singletons are now allowed in union types. E.g. the following is allowed: `object foo; type X = Int | foo.type`.
- A bunch of improvements was made for the type inference system – see, e.g., PRs [#6454](https://github.com/lampepfl/dotty/pull/6454) and [#6467](https://github.com/lampepfl/dotty/pull/6467).
- Improvements to the Scala 2 code support which, in particular, improves Cats support – see PRs [#6494](https://github.com/lampepfl/dotty/pull/6494) and [#6498](https://github.com/lampepfl/dotty/pull/6498).

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.14.0-RC1..0.15.0-RC1` these are:

```
   191  Martin Odersky
   112  Nicolas Stucki
    29  Guillaume Martres
    25  Olivier Blanvillain
    21  Aleksander Boruch-Gruszecki
    17  Anatolii Kmetiuk
    10  Miles Sabin
     9  Liu Fengyun
     8  Aggelos Biboudis
     8  Jentsch
     5  Sébastien Doeraene
     2  Anatolii
     1  Fengyun Liu
     1  Olivier ROLAND
     1  phderome
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://dotty.epfl.ch/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/lampepfl/dotty/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
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
