Scala 3 (https://www.scala-lang.org)
Copyright 2012-2026 EPFL
Copyright 2012-2026 Lightbend, Inc. dba Akka

Licensed under the Apache License, Version 2.0 (the "License"):
http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

The Scala 3 compiler is also known as Dotty. The Dotty compiler
frontend has been developed since November 2012 by Martin Odersky. It
is expected and hoped for that the list of contributors to the
codebase will grow quickly. Dotty draws inspiration and code from the
original Scala 2 compiler "nsc", which is still developed at scala/scala [1].

The majority of the Dotty codebase is new code, with the exception of the
components mentioned below. We have for each component tried to come up with a
list of the original authors in the scala/scala [1] codebase. Apologies if some
major authors were omitted by oversight.

  * dotty.tools.dotc.ast: The syntax tree handling is mostly new, but some
    elements, such as the idea of tree copiers and the `TreeInfo` module,
    were adopted from scala/scala [1]. The original authors of these parts
    include Martin Odersky, Paul Phillips, Adriaan Moors, and Matthias Zenger.

  * dotty.tools.dotc.classpath: The classpath handling is taken mostly as is
    from scala/scala [1]. The original authors were Grzegorz Kossakowski,
    Michał Pociecha, Lukas Rytz, Jason Zaugg and others.

  * dotty.tools.dotc.config: The configuration components were adapted and
    extended from scala/scala [1]. The original sources were authored by Paul
    Phillips with contributions from Martin Odersky, Miguel Garcia and others.

  * dotty.tools.dotc.core: The core data structures and operations are mostly
    new. Some parts (e.g. those dealing with names) were adapted from
    scala/scala [1]. These were originally authored by Martin Odersky, Adriaan
    Moors, Jason Zaugg, Paul Phillips, Eugene Burmako and others.

  * dotty.tools.dotc.core.pickling: The classfile readers were adapted from the
    current Scala compiler. Original authors were Martin Odersky, Iulian
    Dragos, Matthias Zenger and others.

  * dotty.tools.dotc.parsing: The lexical and syntactic analysis components
    were adapted from the current Scala compiler. They were originally authored
    by Martin Odersky, Burak Emir, Paul Phillips, Lex Spoon, Sean McDirmid and
    others.

  * dotty.tools.dotc.profile: The per-phase profiling support is taken mostly
    as is from scala/scala. The original author was Mike Skells.

  * dotty.tools.dotc.reporting: Adapted from scala/scala [1] with some heavy
    modifications. They were originally authored by Matthias Zenger, Martin
    Odersky, and others.

  * dotty.tools.dotc.typer: This is new code except for some minor components
    (e.g. the ConstantFolder). It uses however many solution details that have
    been developed over time by many people, including Jason Zaugg, Adriaan
    Moors, Lukas Rytz, Paul Phillips, Grzegorz Kossakowski, and others.

  * dotty.tools.dotc.util: The utilities package is a mix of new and adapted
    components. The files in scala/scala [1] were originally authored by many
    people, including Paul Phillips, Martin Odersky, Sean McDirmid, and others.

  * dotty.tools.io: The I/O support library was adapted from current Scala
    compiler. Original authors were Paul Phillips and others.

  * dotty.test.DottyBytecodeTest: Is an adaptation of the bytecode testing from
    scala/scala [1]. It has been reworked to fit the needs of dotty. Original
    authors include: Adrian Moors, Lukas Rytz, Grzegorz Kossakowski, Paul
    Phillips.

  * dotty.tools.dotc.sbt and everything in sbt-bridge: The sbt compiler phases
    are based on [2] which attempts to integrate the sbt phases into scalac and
    is itself based on the compiler bridge in sbt 0.13 [3], but has been
    heavily adapted and refactored. Original authors were Mark Harrah, Grzegorz
    Kossakowski, Martin Duhemm, Adriaan Moors and others.

  * dotty.tools.dotc.plugins: Adapted from scala/scala [1] with some
    modifications. They were originally authored by Lex Spoon, Som Snytt,
    Adriaan Moors, Paul Phillips and others.

  * dotty.tools.scaladoc: The Scaladoc documentation utility ships some
    third-party JavaScript and CSS libraries which are located under
    scaladoc/resources/dotty_res/styles/, scaladoc/resources/dotty_res/scripts/, docs/css/ and
    docs/js/. Please refer to the license header of the concerned files for
    details.

  * dotty.tools.dotc.coverage: Coverage instrumentation utilities have been
    adapted from the scoverage plugin for scala 2 [4], which is under the
    Apache 2.0 license.

  * dooty.tools.pc: Presentation compiler implementation adapted from
    scalameta/metals [5] mtags module, which is under the Apache 2.0 license.

  * The Dotty codebase contains parts which are derived from
    the ScalaPB protobuf library [6], which is under the Apache 2.0 license.


[1] https://github.com/scala/scala
[2] https://github.com/adriaanm/scala/tree/sbt-api-consolidate/src/compiler/scala/tools/sbt
[3] https://github.com/sbt/sbt/tree/0.13/compile/interface/src/main/scala/xsbt
[4] https://github.com/scoverage/scalac-scoverage-plugin
[5] https://github.com/scalameta/metals
[6] https://github.com/scala/scala3/pull/5783/files
