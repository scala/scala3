The dotty compiler frontend has been developed since November 2012 by Martin Odersky. It is expected and hoped for
that the list of contributors to the codebase will grow quickly. Dotty draws inspiration and code from the original
Scala compiler "nsc", which is developed at [scala/scala](https://github.com/scala/scala).

The majority of the dotty codebase is new code, with the exception of the components mentioned below. We have for each component tried to come up with a list of the original authors in the  [scala/scala](https://github.com/scala/scala) codebase. Apologies if some major authors were omitted by oversight.

`dotty.tools.dotc.ast`

> The syntax tree handling is mostly new, but some elements, such as the idea of tree copiers and the `TreeInfo` module,
> were adopted from  [scala/scala](https://github.com/scala/scala).
> The original authors of these parts include Martin Odersky, Paul Phillips, Adriaan Moors, and Matthias Zenger.

`dotty.tools.dotc.classpath`

> The classpath handling is taken mostly as is from [scala/scala](https://github.com/scala/scala).
> The original authors were Grzegorz Kossakowski, Michał Pociecha, Lukas  Rytz, Jason Zaugg and others.

`dotty.tools.dotc.config`

> The configuration components were adapted and extended from  [scala/scala](https://github.com/scala/scala).
> The original sources were authored by Paul Phillips with contributions from Martin Odersky, Miguel Garcia and others.

`dotty.tools.dotc.core`

> The core data structures and operations are mostly new. Some parts (e.g. those dealing with names) were adapted from  [scala/scala](https://github.com/scala/scala).
> These were originally authored by Martin Odersky, Adriaan Moors, Jason Zaugg, Paul Phillips, Eugene Burmako and others.

`dotty.tools.dotc.core.pickling`

> The classfile readers were adapted from the current Scala compiler. Original authors were Martin Odersky, Iulian Dragos, Matthias Zenger and others.

`dotty.tools.dotc.parsing`

> The lexical and syntactic analysis components were adapted from the current Scala compiler. They were originally authored by Martin Odersky,
> Burak Emir, Paul Phillips, Lex Spoon, Sean McDirmid and others.

`dotty.tools.dotc.reporting`

> Adapted from  [scala/scala](https://github.com/scala/scala) with some heavy modifications. They were originally authored by Matthias Zenger, Martin Odersky, and others.

`dotty.tools.dotc.typer`

> This is new code except for some minor components (e.g. the ConstantFolder). It uses however many solution details that have been developed over time by many people, including Jason Zaugg, Adriaan Moors, Lukas Rytz, Paul Phillips, Grzegorz Kossakowski, and others.

`dotty.tools.dotc.util`

> The utilities package is a mix of new and adapted components. The files in  [scala/scala](https://github.com/scala/scala) were originally authored by many people,
> including Paul Phillips, Martin Odersky, Sean McDirmid, and others.

`dotty.tools.io`

> The I/O support library was adapted from current Scala compiler. Original authors were Paul Phillips and others.

`dotty.test.DottyBytecodeTest`

> Is an adaptation of the bytecode testing from
> [scala/scala](https://github.com/scala/scala). It has been reworked to fit
> the needs of dotty. Original authors include: Adrian Moors, Lukas Rytz,
> Grzegorz Kossakowski, Paul Phillips

`dotty.tools.dotc.sbt and everything in sbt-bridge/`

> The sbt compiler phases are based on
> https://github.com/adriaanm/scala/tree/sbt-api-consolidate/src/compiler/scala/tools/sbt
> which attempts to integrate the sbt phases into scalac and is itself based on
> the [compiler bridge in sbt 0.13](https://github.com/sbt/sbt/tree/0.13/compile/interface/src/main/scala/xsbt),
> but has been heavily adapted and refactored.
> Original authors were Mark Harrah, Grzegorz Kossakowski, Martin Duhemm, Adriaan Moors and others.
