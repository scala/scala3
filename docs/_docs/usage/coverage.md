---
layout: doc-page
title: "Code Coverage for Scala 3"
---

## Instrument code for coverage analysis

[PR#13880](https://github.com/lampepfl/dotty/pull/13880) has implemented code coverage support for Dotty.
In general, code coverage works in three steps:
1. The program is "instrumented" at compilation time: code is inserted to record which statement are called. This does not change the behavior of the program. Also, a list of all the coverable statements is produced.
2. The program is run, usually by unit tests, and the instrumentation code saves its measurements.
3. Some tool processes the data to generate a fancy coverage report, for instance a web page.

In Scala 2, all these steps were performed by external tools. In particular, step 1 was implemented by a compiler plugin.

In Scala 3, the compiler itself takes care of step 1. To use this feature, add the compile option `-coverage-out:DIR`, where `DIR` is the destination of the measurement files.

You can also set `-coverage-sourceroot:PATHS_ROOT` to customize how the path of your source files are resolved.

## How-to with sbt

For now, the Scoverage sbt plugin doesn't apply the above options automatically.
However, you can easily do it yourself, and use the plugin to generate user-friendly reports.

1. Add the scoverage sbt plugin by appending this line to your `project/plugins.sbt`
```scala
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.0-M4")
```

2. Compile your project with
```scala
Compile/compile/scalacOptions +=
  s"-coverage-out:target/scala-${scalaVersion.value}/scoverage-data"
```

2. Run the tests: `sbt test`
3. Generate xml and html reports: `sbt coverageReport`

## Details: how the code is instrumented

When the `-coverage-out` option is set, a new phase `instrumentCoverage` runs, just before `firstTransform`.
For a carefully selected list of tree types, it adds a call to `scala.runtime.Invoker.invoked(statementId, DIR)`.

For instance, this code:
```
def method() =
  println(f())
```

with `-coverage-out:target/cov` be turned to
```
def method() =
  Invoker.invoked(2, "target/cov")
  println({
    Invoker.invoked(1, "target/cov")
    f()
  })
```

At the end of the phase, the list of all the instrumented statements is serialized to the file `DIR/scoverage.coverage`.
