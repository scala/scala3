dotty
=====

[![Join the chat at https://gitter.im/lampepfl/dotty](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/lampepfl/dotty?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Dotty is a platform to try out new language concepts and compiler
technologies for Scala. The focus is mainly on simplification. We
remove extraneous syntax (e.g. no XML literals), and try to boil down
Scala's types into a smaller set of more fundamental constructors. The
theory behind these constructors is researched in
[DOT](http://www.cs.uwm.edu/~boyland/fool2012/papers/fool2012_submission_3.pdf),
a calculus for dependent object types.

The dotty compiler is largely a new design. It takes a more functional
approach than current scalac, paired with aggressive caching to
achieve good performance. Some parts, in particular those that have to
do with configuration and input/output are ported from the Scala
compiler. The compiler is fully functional, in particular it can
compile itself. But there's more work needed (and planned) on
polishing rough edges, improving diagnostics, improving compilation
speed, and embedding in other tools.

We expect that, over time, some of the new technologies explored in
this project will find their way into future versions of Scala. At
present it is too early to say which ones and when.

If you want to try it out, to get started have a look at
https://github.com/lampepfl/dotty/wiki/Getting-Started.

Developers mailing list is https://groups.google.com/forum/#!forum/dotty-internals.


&nbsp;

&nbsp;

![YourKit](https://www.yourkit.com/images/yklogo.png) supports open source projects with its full-featured Java Profiler.

YourKit, LLC is the creator of [YourKit Java Profiler](https://www.yourkit.com/java/profiler/index.jsp).
