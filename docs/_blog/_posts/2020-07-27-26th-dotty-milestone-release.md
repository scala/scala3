---
layout: blog-page
title: Announcing Dotty 0.26.0-RC1 - unified extension methods and more
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2020-07-27
---

Hello! We are excited to announce 0.26.0-RC1 of Dotty. In this version, we have improved the extension methods – their syntax is now more uniform. We have also implemented local selectable instances and have done a bunch of improvements to the compiler and the language API. Otherwise, we are focusing our efforts on reducing the issue count on the issue tracker, boosting performance and improving the stability of the compiler in other ways.

You can try out this version right now, from the comfort of your SBT, by visiting the [home page](https://dotty.epfl.ch/) and scrolling down to the "Create a Dotty Project" section.

Alternatively, you can try this version of Scala online via [Scastie](https://scastie.scala-lang.org/). Once you're there, click "Build Settings" and set "Target" to "Dotty".

Enjoy the ride🚀!

<!--more-->
# Unified extension methods
In this release, we have made extension method syntax uniform. Previously, we had three separate syntaxes for single extension methods, collective extension methods and given instances with extension methods. Now, these three cases have been unified into one.  The new syntax looks like follows:

```scala
extension (x: String)
  def < (y: String): Boolean = ...
```

Collective extensions look like follows:

```scala
extension (ss: Seq[String]):

  def longestStrings: Seq[String] =
    val maxLength = ss.map(_.length).max
    ss.filter(_.length == maxLength)

  def longestString: String = longestStrings.head
```

You can read more about the new syntax in the [documentation](https://dotty.epfl.ch/docs/reference/contextual/extension-methods.html). For the discussion, see [PR](https://github.com/lampepfl/dotty/pull/9255).

# Local Selectable Instances
Local and anonymous classes that extend `Selectable` get more refined types than other classes. For example:

```scala
trait Vehicle extends reflect.Selectable {
  val wheels: Int
}
val i3 = new Vehicle { // i3: Vehicle { val range: Int }
  val wheels = 4
  val range = 240
}
i3.range
```

Without the `extends reflect.Selectable`, the last line would have errored:

```scala
i3.range: // error: range is not a member of `Vehicle`
```

The new functionality is similar to `scala.Dynamic` but different since `Selectable` is typesafe. For more about this feature, see [documentation](https://dotty.epfl.ch/docs/reference/changed-features/structural-types.html#local-selectable-instances).

# Tuple counterparts for `summon` and `constValue`
Two new methods for compile-time programming were added, `summonAll` and `constValueTuple`.

`summonAll[T <: Tuple]` takes a tuple type, summons all the members of it and returns them as a tuple. For example:

```scala
given as Int = 10
given as String = "foo"
given as Double = 1.2
println(summonAll[Int *: String *: Double *: EmptyTuple])  // (10,foo,1.2)
```

In the same spirit, `constValueTuple[T <: Tuple]` is a tuple counterpart for `constValue`. For example:

```scala
val result = constValueTuple["foo" *: "bar" *: 10 *: 2.5 *: EmptyTuple]
println(result)  // (foo,bar,10,2.5)
```

This feature was introduced by PR [#9209](https://github.com/lampepfl/dotty/pull/9209).

# Per-run time budget for import suggestions
Import suggestions is a feature useful for debugging but potentially taxing for performance. Therefore, we have added the `-Ximport-suggestion-timeout <time-in-ms>` to allow specifying the timeout (in milliseconds) after which the suggestions mechanism should stop the lookup. The timeout budget is per-run (and not per suggestion) which ensures that the performance does not degrade in case of too many suggestions.

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing

Thank you to all the contributors who made this release possible 🎉

According to `git shortlog -sn --no-merges 0.25.0-RC2..0.26.0-RC1` these are:

```
   128  Martin Odersky
    53  Nicolas Stucki
    30  Sébastien Doeraene
    18  Anatolii Kmetiuk
    18  Guillaume Raffin
    17  Lan, Jian
    12  Guillaume Martres
     5  Aleksander Boruch-Gruszecki
     3  Ruslan Shevchenko
     3  odersky
     2  Alden Torres
     2  Robert Stoll
     2  yu-croco
     1  Alex Zolotko
     1  Kevin Dreßler
     1  FabioPinheiro
     1  adpi2
     1  Matthew Pickering
     1  Liu Fengyun
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
