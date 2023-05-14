---
layout: blog-page
title: Announcing Dotty 0.27.0-RC1 - ScalaJS, performance, stability
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2020-08-31
---

Hello! We are excited to announce 0.27.0-RC1 of Dotty. In this version, we bring ScalaJS support to Dotty. As we are getting closer to the Scala 3 release, we continue shifting our focus to stability and performance, which are the central theme of this release.

You can try out this version right now, from the comfort of your SBT, by visiting the [home page](https://dotty.epfl.ch/) and scrolling down to the "Create a Dotty Project" section.

Alternatively, you can try this version of Scala online via [Scastie](https://scastie.scala-lang.org/). Once you're there, click "Build Settings" and set "Target" to "Dotty".

Enjoy the ride🚀!

<!--more-->
# Welcome Scala.js support (with caveats)
This release brings enough support for Scala.js that it should be actually usable in a number of projects.
To use it, make sure of the following:

* Use sbt-scalajs v1.1.1 or later
* Use sbt-dotty v0.4.2 or later
* Use `scalaVersion := "0.27.0-RC1"` or later for Dotty
* Use `enablePlugins(ScalaJSPlugin)` or a `crossProject`

When the above are combined, sbt-scalajs ans sbt-dotty will coordinate to transparently configure your project with Dotty with its Scala.js support.

In this release, the following features are supported:

* Dependencies with `%%%`, including with `withDottyCompat`,
* The entire portable subset of the language, i.e., code that compiles both on the JVM and on JS,
* Calling JavaScript APIs, including those defined in dependencies.

The following features are *not supported yet*:

* Define non-native JS classes (i.e., classes extending `js.Any` but without `@js.native`): they will report compile errors
* Exports of all kinds (i.e., `@JSExportXYZ`): they will be silently ignored

To the best of our knowledge, cross-compiling libraries should be able to use Scala.js with Dotty in plain capacity.
If you experience a bug with anything except the unsupported features mentioned above, please file a bug report.

# Stability
As we're getting closer to the release of Scala 3, we are continuing to focus on the stability and performance of the language. In this release, we have fixed support of objects under JDK9+ (PR [#9181](https://github.com/lampepfl/dotty/pull/9181)). The issue was, due to the changes in JDK9+ compared to JDK8, our initialization scheme for objects did not work under JDK9+. The aforementioned fixed that issue, thereby unblocking JDK9+ support for Dotty.

We are also continuing to work on stabilising enums. PR [#9532](https://github.com/lampepfl/dotty/pull/9532) corrects the deserialization and serialization of singleton enum values with `ObjectInputStream` and `ObjectOutputStream`. PR [#9549](https://github.com/lampepfl/dotty/pull/9549) enables overriding the `toString` method on enums – previously this was not possible because of the way enums were desugared.

# Performance
We are also focusing these days on making the compiler faster and memory-efficient. For the past month, we were looking in the compiler's memory footprint. We were trying to determine what was allocated in unreasonable amounts during compilation and trying to resolve these allocation issues. The following PRs attempt to increase the performance of the compiler:

- Optimize megaphase [#9597](https://github.com/lampepfl/dotty/pull/9597)
- Cache all memberNamed results [#9633](https://github.com/lampepfl/dotty/pull/9633)
- Parallelize position pickling [#9619](https://github.com/lampepfl/dotty/pull/9619)
- Simplify TypeComparer [#9405](https://github.com/lampepfl/dotty/pull/9405)
- Optimize and simplify SourcePosition handling [#9561](https://github.com/lampepfl/dotty/pull/9561)

# Metaprogramming
We are keeping the work on the metaprogramming API improvements. For this release, the following PRs bring better API to metaprogrammers:

- Avoid leak of internal implementation in tasty.Reflection [#9613](https://github.com/lampepfl/dotty/pull/9613)
- Redefine quoted.Expr.betaReduce [#9469](https://github.com/lampepfl/dotty/pull/9469)

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing
Thank you to all the contributors who made this release possible 🎉

According to `git shortlog -sn --no-merges 0.26.0-RC1..0.27.0-RC1` these are:

```
   118  Martin Odersky
    75  Liu Fengyun
    65  Nicolas Stucki
    27  Sébastien Doeraene
    23  Guillaume Martres
    16  Jamie Thompson
    15  bishabosha
    10  Guillaume Raffin
     7  Anatolii Kmetiuk
     4  Robert Stoll
     3  Pavel Shirshov
     3  december32
     3  odersky
     2  ysthakur
     1  Niklas Vest
     1  Dean Wampler
     1  Fengyun Liu
     1  John Sullivan
     1  Lan, Jian
     1  Aleksander Boruch-Gruszecki
     1  Ruslan Shevchenko
     1  Stefan Zeiger
     1  William Narmontas
     1  xuwei-k
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://dotty.epfl.ch/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/lampepfl/dotty/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
They make perfect entry points into hacking on the compiler.

We are looking forward to having you join the team of contributors.

## Library authors: Join our community build

Dotty now has a set of widely-used community libraries that are built against every nightly Dotty
snapshot. Currently, this includes shapeless, ScalaPB, algebra, scalatest, scopt and squants.
Join our [community build](https://github.com/lampepfl/dotty/tree/main/community-build)
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
