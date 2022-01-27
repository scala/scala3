---
layout: blog-page
author: Martin Odersky and Dmitry Petrashko
title: "We got liftoff!"
subTitle: The Dotty compiler for Scala bootstraps.
excerpt_separator: <!--more-->
date: 2015-10-23
---

The [Dotty project](https://github.com/lampepfl/dotty)
is a platform to develop new technology for Scala
tooling and to try out concepts of future Scala language versions.
Its compiler is a new design intended to reflect the
lessons we learned from work with the Scala compiler. A clean redesign
today will let us iterate faster with new ideas in the future.

Today we reached an important milestone: the Dotty compiler can
compile itself, and the compiled compiler can act as a drop-in for the
original one. This is what one calls a *bootstrap*.

<!--more-->

## Why is this important?

The main reason is that this gives us a some validation of the
*trustworthiness* of the compiler itself. Compilers are complex beasts,
and many things can go wrong. By far the worst things that can go
wrong are bugs where incorrect code is produced. It's not fun debugging code that looks perfectly
fine, yet gets translated to something subtly wrong by the compiler.

Having the compiler compile itself is a good test to demonstrate that
the generated code has reached a certain level of quality. Not only is
a compiler a large program (44k lines in the case of Dotty), it is
also one that exercises a large part of the language in quite
intricate ways. Moreover, bugs in the code of a compiler don't tend to
go unnoticed, precisely because every part of a compiler feeds into
other parts and all together are necessary to produce a correct
translation.

## Are we done yet?

Far from it! The compiler is still very rough. A lot more work is
needed to

 - make it more robust, in particular when analyzing incorrect programs,
 - improve error messages and warnings,
 - improve the efficiency of some of the generated code,
 - improve compilation speed,
 - embed it in external tools such as sbt, REPL, IDEs,
 - remove restrictions on what Scala code can be compiled,
 - help in migrating Scala code that will have to be changed.

## What are the next steps?

Over the coming weeks and months, we plan to work on the following topics:

 - Make snapshot releases.
 - Work on SBT integration of the compiler.
 - Work on IDE support.
 - Investigate the best way to obtaining a REPL.
 - Work on the build infrastructure.

If you want to get your hands dirty with any of this, now is a good
moment to get involved! Join the team of contributors, including
Dmitry Petrashko ([@DarkDimius](https://github.com/DarkDimius)),
Guillaume Martres ([@smarter](https://github.com/smarter)),
Ondrey Lhotak ([@olhotak](https://github.com/olhotak)),
Samuel Gruetter ([@samuelgruetter](https://github.com/samuelgruetter)),
Vera Salvis ([@vsalvis](https://github.com/vsalvis)),
and Jason Zaugg ([@retronym](https://github.com/retronym)).

To get started: <https://github.com/lampepfl/dotty>.
