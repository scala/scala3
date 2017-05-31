---
layout: blog-page
title: Announcing Dotty 0.1.2-RC1, a major step towards Scala 3
author: Dmytro Petrashko
authorImg: /images/petrashko.jpg
---

Today, we excited to release Dotty version 0.1.2-RC1.  This release
serves as a technology preview that demonstrates new language features
and the compiler supporting them.


<!--more-->

## Why is this important?

_Dotty_ is the project name for a language and compiler that
is slated to become Scala 3.0. This is an ongoing development,
developed transparently as open source software. The Dotty project
started more than 4 years ago. It reached a major milestone in 2015 by
achieving
[bootstrap](http://dotty.epfl.ch/blog/2015/10/23/dotty-compiler-bootstraps.html),
that is, showing that the new compiler could compile itself.  Today we
have reached another milestone with this first release of the
codebase. Developments will not stop here, but they will in the future
all be part of regular time-based releases.

## How can you try it out?

You have several alternatives: use the `sbt-dotty` plugin, get a standalone
installation, or try it online on [Scastie](https://scastie.scala-lang.org/?target=dotty).

### sbt
Using sbt 0.13.13 or newer, do:

```
sbt new lampepfl/dotty.g8
```

This will setup a new sbt project with Dotty as compiler. For more details on
using Dotty with sbt, see the
[example project](https://github.com/lampepfl/dotty-example-project).

### Standalone installation

Releases are available for download on the _Releases_
section of the Dotty repository: https://github.com/lampepfl/dotty/releases.

We also provide a [homebrew](https://brew.sh/) package that can be installed by running

```
brew install lampepfl/brew/dotty
```

### Scastie

[Scastie](https://scastie.scala-lang.org/?target=dotty), the online Scala playground,
supports Dotty.


# What’s in the 0.1.2-RC1 technology preview?
This technology preview demonstrates new language features planned for Scala 3:

  - [Intersection Types](http://dotty.epfl.ch/docs/reference/intersection-types.html)
  - [Union Types](http://dotty.epfl.ch/docs/reference/union-types.html)
  - [Trait Parameters](http://dotty.epfl.ch/docs/reference/trait-parameters.html)
  - [Enumerations](http://dotty.epfl.ch/docs/reference/enums.html)
  - [Algebraic Data Types](http://dotty.epfl.ch/docs/reference/adts.html)
  - [By-Name Implicits](http://dotty.epfl.ch/docs/reference/implicit-by-name-parameters.html)

We also ship with tools that help you try out the Dotty platform:

  - [IDE features for Visual Studio Code](http://dotty.epfl.ch/docs/usage/ide-support.html)
  - [sbt support, including retro-compatibility with Scala 2](https://github.com/lampepfl/dotty-example-project)

See here for the full [release notes](http://dotty.epfl.ch/docs/release-notes/0.1.2.html).

## Release schedule

We are adopting a time-based release schedule:
- Nightly builds will be published, for those wanting to be at the forefront of
  development.
- Every 6 weeks, a release candidate will be cut based on the last nightly build,
  the release candidates let library authors test their code in advance of each
  release. Multiple release candidates may be released during each 6 weeks
  period to fix regressions.
- Every 6 weeks, the last release candidate becomes a release.

At the end of 6 weeks, the milestone will be promoted to a stable release.
See here for the full [version number explanation](http://dotty.epfl.ch/docs/usage/version-numbers.html).

## What are the next steps?

Over the coming weeks and months, we plan to work on the following topics:

 - [Integrate Local optimizations developed in Dotty linker](https://github.com/lampepfl/dotty/pull/2513);
 - [Add Language-level support for HMaps and HLists](https://github.com/lampepfl/dotty/pull/2199);
 - [Port global optimizations from Dotty linker](https://github.com/lampepfl/dotty/pull/1840).

If you want to get your hands dirty with any of this, now is a good
moment to get involved! Join the team of contributors, including
Martin Odersky ([@odersky](https://twitter.com/odersky))
Dmitry Petrashko ([@DarkDimius](https://twitter.com/DarkDimius)),
Guillaume Martres ([@smarter](https://github.com/smarter)),
Felix Mulder ([@felixmulder](https://twitter.com/felixmulder)),
Nicolas Stucki ([@nicolasstucki](https://github.com/nicolasstucki)),
Liu Fengyun ([@liufengyun](https://github.com/liufengyun)),
Olivier Blanvillain ([@OlivierBlanvillain](https://github.com/OlivierBlanvillain)),
and others!

## Library authors: Join our community build

Dotty now has a set of libraries that are built against every nightly snapshot.
Currently this includes scalatest, squants and algebra.
Join our [community build](https://github.com/lampepfl/dotty-community-build)
 to make sure that our regression suite includes your library.


To get started, see <https://github.com/lampepfl/dotty>.
