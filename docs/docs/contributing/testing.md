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
$ sbt
> dotty-compiler/test
```

To run a single test class you use `testOnly` and the fully qualified class name. 
For example:

```bash
> testOnly dotty.tools.dotc.transform.TreeTransformerTest
```

The test command follows a regular expression-based syntax `testOnly * -- *`. 
The right-hand side picks a range of names for methods and the left-hand side picks a range of class names and their
fully-qualified paths.

Consequently, you can restrict the aforementioned executed test to a subset of methods by appending ``-- *method_name``.
The example below picks up all methods with the name `canOverwrite`:

```bash
> testOnly dotty.tools.dotc.transform.TreeTransformerTest -- *canOverwrite
```

Additionally, you can run all tests named `method_name`, in any class, without providing a class name:

```bash
> testOnly -- *canOverwrite
```

You can also run all paths of classes of a certain name:

```bash
> testOnly *.TreeTransformerTest
```

## Integration tests
These tests are Scala source files expected to compile with Dotty (pos tests),
along with their expected output (run tests) or errors (neg tests).

All of these tests are contained in the `./tests/*` directories and can be run with the `vulpix` command.

Currently to run these tests you need to invoke from sbt:

```bash
$ sbt
> vulpix
```

(which is effectively the same with `testOnly dotty.tools.dotc.CompilationTests`)

It is also possible to run tests filtered, again from sbt:

```bash
$ sbt
> vulpix i2147.scala
```

This will run both the test `./tests/pos/i2147.scala` and
`./tests/partest-test/i2147.scala` since both of these match the given string.
This also means that you could run `vulpix` with no arguments to run all integration tests.
