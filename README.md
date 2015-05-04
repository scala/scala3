dotty
=====

[![Build Status](https://travis-ci.org/lampepfl/dotty.png?branch=master)](https://travis-ci.org/lampepfl/dotty)

Dotty is a platform to try out new language concepts and compiler technologies for Scala. The focus is mainly on simplification. We remove extraneous syntax (e.g. no XML literals), and try to boil down Scala's types into a smaller set of more fundamental constructors. The theory behind these constructors is researched in [DOT](http://www.cs.uwm.edu/~boyland/fool2012/papers/fool2012_submission_3.pdf), a calculus for dependent object types. 

The dotty compiler is largely a new design. It takes a more functional approach than current scalac, paired with aggressive caching to achieve good performance. At present, only the frontend (parser and type-checker) exists; the transformation and code generation phases remain to be written. Some parts, in particular those that have to do with configuration and input/output are derived from the Scala compiler.

We expect that, over time, some of the new technologies explored in this project will find their way into future versions of Scala. At present it is too early to say which ones and when.

If you want to try it out, to get started have a look at https://github.com/lampepfl/dotty/wiki/Getting-Started.

Developers mailing list is https://groups.google.com/forum/#!forum/dotty-internals.


&nbsp;

&nbsp;

![YourKit](https://www.yourkit.com/images/yklogo.png) supports open source projects with its full-featured Java Profiler.

YourKit, LLC is the creator of [YourKit Java Profiler](https://www.yourkit.com/java/profiler/index.jsp).
