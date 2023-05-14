---
layout: blog-page
title: Announcing Dotty 0.4.0-RC1
author: Allan Renucci
authorImg: images/allan.jpg
date: 2017-10-16
---

Today, we are excited to release Dotty version 0.4.0-RC1. This release
serves as a technology preview that demonstrates new language features
and the compiler supporting them.

If you’re not familiar with Dotty, it's a platform to try out new language concepts and compiler
technologies for Scala. The focus is mainly on simplification. We remove extraneous syntax
(e.g. no XML literals), and try to boil down Scala’s types into a smaller set of more fundamental
constructs. The theory behind these constructs is researched in
[DOT](https://infoscience.epfl.ch/record/215280), a calculus for dependent object types.
You can learn more about Dotty on our [website](http://dotty.epfl.ch).

<!--more-->

This is our fourth scheduled release according to our [6-week release schedule](https://dotty.epfl.ch/docs/usage/version-numbers.html).
The [previous technology preview](/_blog/_posts/2017-09-07-third-dotty-milestone-release.html) improved
stability and reliability.

## What’s new in the 0.4.0-RC1 technology preview?

### Rewritten REPL [#2991](https://github.com/lampepfl/dotty/pull/2991)
The original Dotty REPL was a proof of concept hacked together
from
[an ancient version of the scalac REPL](https://github.com/lampepfl/dotty/pull/1082#issuecomment-183905504).
It worked by creating Scala source files from the user input using string
concatenation, this made it easy to adapt it for Dotty since it did not rely on
the internals of scalac, but it was also fragile and hard to reason about.
The [new REPL](https://github.com/lampepfl/dotty/pull/2991) instead works by
manipulating ASTs (Abstract Syntax Trees), this is more robust and will make it
easier to develop new features: we have already implemented auto-completion
support (by reusing the APIs we had created for
the Dotty IDE and we have plans for displaying API documentation in the REPL.

Note that the user interface of the REPL has not changed: like in the old REPL
we use code adapted from the [Ammonite REPL](http://ammonite.io/#Ammonite-REPL)
to provide syntax highlighting, multi-line editing, history, etc.

### Scala 2.12 support [#2827](https://github.com/lampepfl/dotty/pull/2827)
Since our first release, it has been possible to use Scala 2 libraries in a
Dotty project as explained in the
[dotty-example-project](https://github.com/smarter/dotty-example-project#getting-your-project-to-compile-with-dotty).
Previously, we supported libraries compiled by Scala 2.11, but starting with this
release we support Scala 2.12 instead. If your Dotty project has Scala 2
dependencies this change should be transparent for you assuming all your
dependencies have been published for 2.12.

### Performance work
Over the last few weeks, we started working on compilation speed with some good results:
compiling [ScalaPB](https://github.com/dotty-staging/scalapb) is now 20% faster
than with Dotty 0.3.0-RC2. You can follow along our progress on
http://dotty-bench.epfl.ch/.


## Trying out Dotty
### Scastie
[Scastie], the online Scala playground, supports Dotty.
This is an easy way to try Dotty without installing anything.

### sbt
Using sbt 0.13.13 or newer, do:

```
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
[https://github.com/lampepfl/dotty/releases](https://github.com/lampepfl/dotty/releases)

We also provide a [homebrew](https://brew.sh/) package that can be installed by running:

```
brew install lampepfl/brew/dotty
```

In case you have already installed Dotty via brew, you should instead update it:

```
brew upgrade dotty
```

## Let us know what you think!
If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing
Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.3.0-RC2..0.4.0-RC1` these are:

```
   226  Martin Odersky
   112  Felix Mulder
   104  Nicolas Stucki
    41  Allan Renucci
    41  Guillaume Martres
    33  liu fengyun
     8  Olivier Blanvillain
     4  Aggelos Biboudis
     3  Dmitry Petrashko
     2  Raymond Tay
     2  esarbe
     2  Enno Runne
     1  Brandon Elam Barker
     1  Raphael Bosshard
     1  Jacob J
     1  Aleksander Boruch-Gruszecki
     1  Jim Van Horn
     1  Matthias Sperl
     1  Michal Gutowski
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
You can have a look at our [Getting Started page for new contributors](https://dotty.epfl.ch/docs/contributing/getting-started.html),
the [Awesome Error Messages](http://scala-lang.org/blog/2016/10/14/dotty-errors.html) project or some of
the simple [Dotty issues](https://github.com/lampepfl/dotty/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
They make perfect entry-points into hacking on the compiler.

We are looking forward to having you join the team of contributors.

## Library authors: Join our community build
Dotty now has a set of widely-used community libraries that are built against every nightly Dotty
snapshot. Currently this includes ScalaPB, algebra, scalatest, scopt and squants.
Join our [community build](https://github.com/lampepfl/dotty-community-build)
to make sure that our regression suite includes your library.

To get started, see [https://github.com/lampepfl/dotty](https://github.com/lampepfl/dotty).


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
