---
layout: blog-page
title: Announcing Dotty 0.1.2, a major step towards Scala 3
author: Dmytro Petrashko
authorImg: /images/petrashko.jpg
---

Today, we’re excited to release the Dotty version 0.1.2-RC1. 
This release serves as a technology preview that demonstrates new language features and compiler supporting them.


<!--more-->

## Why is this important?

17 month has passed since dotty has [bootstrapped](http://dotty.epfl.ch/blog/2015/10/23/dotty-compiler-bootstraps.html).
Back at the time Dotty was just an ambitious experiment.
This experiment has proved to be a major success.
Today, we are glad to announce that Scala 3.0 will be build on Dotty code.

## How you can try it out?

You have two alternatives: either use sbt-dotty plugin or get a standalone installation.

In case you prefer Sbt, We provide Sbt project templates. 
Using sbt version 0.13.13 or newer, do `sbt new lampepfl/dotty.g8`.
This should set-up a new Sbt project with Dotty as compiler.
For more details on how to use sbt-dotty plugin, please have a look
at [example project](https://github.com/lampepfl/dotty-example-project)


In case you prefer to get a standalone `dotc` script,
releases are available for download on the _Releases_
section of Dotty repository: https://github.com/lampepfl/dotty/releases/
We also provide a [homebrew](https://brew.sh/) package that can be installed by running

```
brew install lampepfl/brew/dotty
```

  
# What’s in 0.1.2 technology preview?
The technology preview presents you new language features
 that are we have been developing in preparation for Scala 3: 
 
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

We are adopting a time based release schedule.
Every day we will publish a nightly, for those who want to be on the very edge of progress. 

Every 6 weeks we will cut a new milestone release. 
Those releases are a good place to be for library authors
 as we intend to provide hot-fixes and fix regression there.
 
At the end of 6 weeks, the milestone will be promoted to a stable release.
See here for full [version number explanation](http://dotty.epfl.ch/docs/usage/version-numbers.html).

## What are the next steps?

Over the coming weeks and months, we plan to work on the following topics:

 - [Integrate Local optimizations developed in Dotty linker](https://github.com/lampepfl/dotty/pull/2513);
 - [Add Language-level support for HMaps and HLists](https://github.com/lampepfl/dotty/pull/2199);
 - [Port global optimizations from Dotty linker](https://github.com/lampepfl/dotty/pull/1840).

If you want to get your hands dirty with any of this, now is a good
moment to get involved! Join the team of contributors, including
Dmitry Petrashko ([@DarkDimius](https://twitter.com/DarkDimius)),
Guillaume Martres ([@smarter](https://github.com/smarter)),
Felix Mulder ([@felixmulder](https://twitter.com/felixmulder)),
Nicolas Stucki ([@stucki_nicolas](https://twitter.com/stucki_nicolas)),
Liu Fengyun ([@liufengyun](https://github.com/liufengyun)),
Olivier Blanvillain ([@OlivierBlanvillain](https://github.com/OlivierBlanvillain)),
and others!


To get started: <https://github.com/lampepfl/dotty>.
