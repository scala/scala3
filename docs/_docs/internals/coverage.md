---
layout: doc-page
title: "Code Coverage for Scala 3"
---

## Instrument code for coverage analysis

[PR#13880](https://github.com/lampepfl/dotty/pull/13880) has implemented code coverage support for Dotty.
In general, code coverage "instruments" the program at compile time: code is inserted to record which statement are called. This does not change the behavior of the program. Also, a list of all the coverable statements is produced.

To use this feature, add the compile option `-coverage-out:DIR`, where `DIR` is the destination of the measurement files.

You can also set `-sourceroot:PATHS_ROOT` to customize how the path of your source files are resolved.
Note that `-sourceroot` also sets the root path of the SemanticDB files.

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
