---
layout: doc-page
title: Testing in Dotty
---

Running all tests in Dotty is as simple as:

```bash
$ sbt test
```

There are currently several forms of tests in Dotty. These can be split into
two categories:

## Unit tests
These tests can be found in `<sub-project>/test` and are used to check
functionality of specific parts of the codebase in isolation e.g: parsing,
scanning and message errors.

To run all tests in e.g., for the compiler test-suite you can write:

```bash
> dotty-compiler/test
```

To run a single test class you use `testOnly` and the fully qualified class name. 
For example:

```bash
> dotty-compiler/testOnly dotty.tools.dotc.transform.TreeTransformerTest
```

You can further restrict the executed tests to a subset of methods by appending ``-- *method_name`` 
as in the example below:

```bash
> dotty-compiler/testOnly dotty.tools.dotc.transform.TreeTransformerTest -- *canOverwrite
```

## Integration tests
These tests are Scala source files expected to compile with Dotty (pos tests),
along with their expected output (run tests) or errors (neg tests).

All of these tests are contained in the `./tests/*` directories.

Currently to run these tests you need to invoke from sbt:

```bash
> testOnly dotty.tools.dotc.CompilationTests
```

It is also possible to run tests filtered by using the `vulpix` command, again from sbt:

```bash
> vulpix i2147.scala
```

This will run both the test `./tests/pos/i2147.scala` and
`./tests/partest-test/i2147.scala` since both of these match the given string.
This also means that you could run `vulpix` with no arguments to run all integration tests.
