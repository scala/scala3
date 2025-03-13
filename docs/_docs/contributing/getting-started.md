---
layout: doc-page
title: Getting Started
redirectFrom: /docs/contributing/workflow.html
---

## Scala CLA

Sometime before submitting your pull request you'll want to make sure you have
signed the [Scala CLA][scala-cla]. You can read more about why we require a CLA
and what exactly is included in it [here][scala-cla].

## Making sure the team is aware

Before digging into an issue or starting on a new feature it's a good idea to
make sure an [issue][dotty-issue] or a [discussion][dotty-discussion] has been
created outlining what you plan to work on. This is both for your and the team's
benefit. It ensures you get the help you need, and also gives the compiler team
a heads-up that someone is working on an issue.

For some small changes like documentation, this isn't always necessary, but it's
never a bad idea to check.

## Requirements

- [git] is essential for managing the Scala 3 code, and contributing to GitHub,
  where the code is hosted.
- A Java Virtual Machine (JDK 8 or higher), required for running the build tool.
    - There are multiple ways to get and manage different Java versions. Some
      popular options are:
        - [SDKMAN!](https://sdkman.io/)
        - [Coursier](https://get-coursier.io/docs/cli-java)
        - Or just download Java directly from [Oracle Java 8][java8], [Oracle
          Java 11][java11], or [AdoptOpenJDK 8/11][adopt]. Refer to [JDK
          Compatibility][compat] for Scala/Java compatibility detail.
  - Verify that the JVM is installed by running the following command in a terminal: `java -version`.
- [sbt][sbt-download], the build tool required to build the Scala 3 compiler and libraries.

## Nice To Have

An IDE, such as [Metals] will help you develop in Scala 3 with features such as
autocompletion, code navigation, debugging, and interactive worksheets.

Another popular options is [IntelliJ IDEA for
Scala](https://www.jetbrains.com/help/idea/discover-intellij-idea-for-scala.html).

## Compiling and Running

Start by cloning the repository:

```bash
$ git clone https://github.com/scala/scala3.git
$ cd scala3
```

Dotty provides a standard sbt build: compiling, running and starting a repl can
all be done from within sbt:

```bash
$ sbt
> scalac tests/pos/HelloWorld.scala
> scala HelloWorld
hello world
```

There are also bash scripts that can be used in the same way. Assuming that you
have cloned the Dotty repo locally, append the following line on your
`.bash_profile`, or equivalent for your shell:

```shell
$ export PATH=<path to cloned dotty>/bin:$PATH
```

and you will be able to run the corresponding commands directly from your console:

```shell
# Compile code using Dotty
$ scalac tests/pos/HelloWorld.scala

# Run it with the proper classpath
$ scala HelloWorld
```

## Starting a REPL

```bash
$ sbt
> repl
Welcome to Scala.next (pre-alpha)  (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_101).
Type in expressions to have them evaluated.
Type :help for more information.
scala>
```

or via bash:

```bash
$ scala
```

## Publish to local repository

To test our cloned compiler on local projects:

```bash
$ sbt publishLocal
```
Then in the `build.sbt` file of a test project:

```bash
ThisBuild / scalaVersion := "<dotty-version>-bin-SNAPSHOT"
```
where `dotty-version` can be found in the file `project/Build.scala`, like `3.0.0-M2`


## Generating Documentation

To generate this page and other static page docs, run

```bash
$ sbt
> scaladoc/generateScalaDocumentation
```

For more information, see the [scaladoc section](./scaladoc.md).

## Community

The main development discussion channels are:
- [github.com/lampepfl/dotty/discussions](https://github.com/lampepfl/dotty/discussions)
- [contributors.scala-lang.org](https://contributors.scala-lang.org)
- [gitter.im/scala/contributors](https://gitter.im/scala/contributors)

[git]: https://git-scm.com
[Metals]: https://scalameta.org/metals/
[vs-code]: https://code.visualstudio.com
[lampepfl/dotty]: https://github.com/lampepfl/dotty
[sbt-download]: https://www.scala-sbt.org/download.html
[java8]: https://www.oracle.com/java/technologies/javase-jdk8-downloads.html
[java11]: https://www.oracle.com/java/technologies/javase-jdk11-downloads.html
[adopt]: https://adoptopenjdk.net/
[compat]: https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html
[scala-cla]: https://contribute.akka.io/cla/scala
[dotty-issue]: https://github.com/scala/scala3/issues
[dotty-discussion]: https://github.com/scala/scala3/discussions
