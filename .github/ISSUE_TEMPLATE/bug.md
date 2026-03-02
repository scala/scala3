---
name: "\U0001F41B Bug report"
about: Report a bug about the Dotty Compiler
title: ''
labels: itype:bug, stat:needs triage
assignees: ''

---

## Compiler version

If you're not sure which version you're using, run `print scalaVersion` from sbt.
(If you're running scalac manually, use `scalac -version` instead.)

If possible, check if your issue appears in the nightly version of the compiler! For example, in Scala CLI (the `scala`/`scala-cli` runner script), you can use `//> using scala 3.nightly` (or `-S 3.nightly` from the command line) to grab the latest one.

## Minimized code

<!--
This code should be self contained, compilable (with possible failures) and as small as possible.

Ideally, we should be able to just copy this code to a file and run `scalac` (and maybe `scala`) to reproduce the issue.

If the code has external dependencies, please provide the Scala CLI directives (or SBT/other build tool configuration) that describe them.
Also note that it's easier and faster for the maintenance team to address issues minimised to reproduce bugs without external dependencies.

It's most convenient to also include `using` directives for the Scala version that demonstrates the problem,
any compiler command-line options, as well as dependencies. An example is provided.

It's also fine to paste the transcript of a REPL session. Note that some bugs may be specific to the REPL.
-->

```scala
//> using scala 3.7.0
//> using options -Wall -Werror
//> using dep com.outr::scribe:3.16.1

@main def test = println("hello, world")
```

## Output

```scala
// TODO add output here
```

## Expectation
