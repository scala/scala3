---
layout: blog-page
title: Announcing Dotty 0.1.2-RC1, a major step towards Scala 3
author: Dmytro Petrashko
authorImg: /images/petrashko.jpg
---

Today, we’re excited to release the first alpha version of Dotty, version 0.1.2-RC1.
This release is a technology preview demonstrating new language features, we are
interested in feedback from both application and library authors willing to
experiment with it.


<!--more-->

## Why is this important?

17 months have passed since Dotty first [bootstrapped](http://dotty.epfl.ch/blog/2015/10/23/dotty-compiler-bootstraps.html).
Back then, Dotty was just an ambitious experiment, but we're now feeling
confident that the underlying technology is solid, and thus are ready to
announce that Dotty will be the basis of Scala 3.0.

## How can you try it out?

You have several alternatives: use the `sbt-dotty` plugin, get a standalone
installation, or try it online on [Scastie](https://scastie.scala-lang.org/).

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

[Scastie](https://scastie.scala-lang.org/), the online Scala playground,
supports Dotty. To try it out:
1. Click on `Build Settings` in the menu on the left
2. Under `Target`, click on `Dotty`
3. That's it!

  
# What’s in the 0.1.2-RC1 technology preview ?
This technology preview demonstrates new language features planned for Scala 3:
 
  - [Intersection Types](http://dotty.epfl.ch/docs/reference/intersection-types.html)
  - [Union Types](http://dotty.epfl.ch/docs/reference/union-types.html)
  - [Trait Parameters](http://dotty.epfl.ch/docs/reference/trait-parameters.html)
  - [Enumerations](http://dotty.epfl.ch/docs/reference/enums.html)
  - [Algebraic Data Types](http://dotty.epfl.ch/docs/reference/adts.html)
  - [By-Name Implicits](http://dotty.epfl.ch/docs/reference/implicit-by-name-parameters.html)

We also ship with tools that help you try out the Dotty platform:

  - [Visual Studio Code Plugin](http://dotty.epfl.ch/docs/usage/ide-support.html)
  - [SBT support, including crosscompilation with Scala2](https://github.com/lampepfl/dotty-example-project)

See here for full [release notes](http://dotty.epfl.ch/docs/release-notes/0.1.2.html).

## Release schedule

We have decided to adopt a time-based release schedule:
- Nightly builds will be published, for those wanting to be at the forefront of
  development.
- Every 6 week, a release candidate will be cut based on the last nightly build,
  the release candidates let library authors test their code in advance of each
  release. Multiple release candidates may be released during each 6 weeks
  period to fix regressions.
- Every 6 week, the last release candidate becomes a release.

Today, we are releasing the first release candidate: 0.1.2-RC1. In 6 weeks, we
will release 0.1.2 final, as well as 0.2.0-RC1.

## What are the next steps ?

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
Nicolas Stucki ([@stucki_nicolas](https://twitter.com/stucki_nicolas)),
Liu Fengyun ([@liufengyun](https://github.com/liufengyun)),
Olivier Blanvillain ([@OlivierBlanvillain](https://github.com/OlivierBlanvillain)),
and others!


To get started, see <https://github.com/lampepfl/dotty>.
