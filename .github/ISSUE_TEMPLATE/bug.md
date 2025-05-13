---
name: "\U0001F41B Bug report"
about: Report a bug about the Dotty Compiler
title: ''
labels: itype:bug, stat:needs triage
assignees: ''

---

## Compiler version

If you're not sure what version you're using, run `print scalaVersion` from sbt
(if you're running scalac manually, use `scalac -version` instead).

If possible, check if your issue appears in the nightly version of the compiler! For example, in scala-cli, you can use `//> using scala 3.nightly` to grab the latest one.

## Minimized code

<!--
This code should be self contained, compilable (with possible failures) and as small as possible.

Ideally, we should be able to just copy this code in a file and run `scalac` (and maybe `scala`) to reproduce the issue.
-->

```scala
println("hello, world")
```

## Output

```scala
// TODO add output here
```

## Expectation
