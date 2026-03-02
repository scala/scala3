---
layout: blog-page
title: Announcing Dotty 0.17.0-RC1 – new implicit scoping rules and more
author: Anatolii Kmetiuk
date: 2019-07-25
---

Greetings! With this post, we are proud to announce the 17th release of Dotty. With this release, we are making steady progress on the metaprogramming capabilities of Scala 3. Also, implicit scoping rules have seen a rework and a bunch of organizational changes took place.

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

This is our 17th scheduled release according to our
[6-week release schedule](https://nightly.scala-lang.org/docs/contributing/procedures/release.html).

# What’s new in the 0.17.0-RC1 technology preview?
## New implicit scoping rules
We aim to make the implicit scoping rules clean and intuitive. In this release, the scoping rules were refactored to facilitate this goal. As specified in the [code documentation](https://github.com/scala/scala3/pull/6832/files#diff-584b631c45ba6f2d4bc5d803074b8f12R474):

The implicit scope of a type `tp` is the smallest set S of object references (i.e. TermRefs
with Module symbol) such that:

- If `tp` is a class reference, S contains a reference to the companion object of the class,
  if it exists, as well as the implicit scopes of all of `tp`'s parent class references.
- If `tp` is an opaque type alias `p.A` of type `tp'`, S contains a reference to an object `A` defined in the
  same scope as the opaque type, if it exists, as well as the implicit scope of `tp'`.
- If `tp` is a reference `p.T` to a class or opaque type alias, S also contains all object references
  on the prefix path `p`. Under Scala-2 mode, package objects of package references on `p` also
  count towards the implicit scope.
- If `tp` is a (non-opaque)  alias of `tp'`, S contains the implicit scope of `tp'`.
- If `tp` is a singleton type, S contains the implicit scope of its underlying type.
- If `tp` is some other type, its implicit scope is the union of the implicit scopes of
  its parts (parts defined as in the spec).

You can learn more from PR [#6832](https://github.com/scala/scala3/pull/6832).

## Metaprogramming
We are making steady progress developing metaprogramming features. The highlights for this release are:

- Tasty Reflection's `Reflection` object moved inside `QuoteContext` object. This means that if previously to do Tasty Reflection you had to implicitly depend on `Reflection`, now you need to depend on `QuoteContext`. To know more, see [#6723](https://github.com/scala/scala3/pull/6723).
- Progress made on quoted patterns – see [#6504](https://github.com/scala/scala3/pull/6504).
- `code` string interpolator allows to obtain the code a user passes to a macro as a String. See [#6661](https://github.com/scala/scala3/pull/6661). To enable this feature, do the following import: `import scala.compiletime._`.

## 2.12 build removed from the CI tests
2.12 build is removed from the test suite. The 2.12 build compiled and tested the Dotty compiler with the Scala 2.12 compiler. This means that, even though Dotty is bootstrapped (i.e. capable of compiling itself), we were not able to use any of the new Dotty features in the Dotty codebase since these features would not compile with Scala 2.12. The decision to abstain from using the new features was made to give us the time to see if something goes wrong with the bootstrap and the ability to revert to Scala 2.12 if it becomes necessary.

The removal of 2.12 build marks the stage in Dotty's life when we start to actively use new Dotty features in our code base, making it incompatible with Scala 2.

## Other changes
There were some organizational and infrastructural changes worth mentioning.

- [Shapeless 3](https://github.com/milessabin/shapeless/tree/shapeless-3) was added to the community build. This means that all the new Dotty features are now tested for the ability to compile Shapeless 3 with them.
- The process of submitting issues to the Dotty issue tracker is standardized using a GitHub issue template. We have separate templates for bugs, compiler crashes and language feature requests. The latter are now not allowed in the main Dotty repository, and the template for feature requests redirects users to a separate repo meant solely for such requests.
- Dotty Knowledge Collection initiative. To improve the documentation of the compiler, we came up with an idea of a separate repo where we will log the raw, unrefined knowledge about the compiler internals. This e.g. can be a quick catch we learnt while working that we believe is worth saving somewhere. To read more about the idea, see the [dotty-knowledge](https://github.com/lampepfl/dotty-knowledge) repo's README.
- `f`-interpolator was implemented as a macro – see the [Scala 2 documentation](https://docs.scala-lang.org/overviews/core/string-interpolation.html#the-f-interpolator) to learn more about what it is.

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.16.0-RC3..0.17.0-RC1` these are:

```
   119  Martin Odersky
   103  Nicolas Stucki
    38  Liu Fengyun
    33  Guillaume Martres
    33  Sara Alemanno
    17  Anatolii
    10  Aggelos Biboudis
     6  Miles Sabin
     5  Anatolii Kmetiuk
     4  Olivier Blanvillain
     4  Robert Stoll
     3  odersky
     2  Dale Wijnand
     2  Timothée Floure
     2  Rodrigo Fernandes
     2  James Thompson
     2  Steven Heidel
     1  Stéphane MICHELOUD
     1  bishabosha
     1  noti0na1
     1  Daniel Reigada
     1  Haemin Yoo
     1  Bunyod
     1  Deon Taljaard
     1  Ondra Pelech
     1  Jon Pretty
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://nightly.scala-lang.org/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/scala/scala3/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
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
