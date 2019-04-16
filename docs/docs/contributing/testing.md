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

All of these tests are contained in the `./tests/*` directories and can be run with the `testCompilation` command. Tests in folders named `with-compiler` are an exception, see next section.

Currently to run these tests you need to invoke from sbt:

```bash
$ sbt
> testCompilation
```

(which is effectively the same with `testOnly dotty.tools.dotc.CompilationTests`)

It is also possible to run tests filtered, again from sbt:

```bash
$ sbt
> testCompilation companions
```

This will run both the test `./tests/pos/companions.scala` and
`./tests/neg/companions.scala` since both of these match the given string.
This also means that you could run `testCompilation` with no arguments to run all integration tests.

When complex checkfiles must be updated, `testCompilation` can run in a mode where it overrides the checkfiles with the test outputs.
```bash
$ sbt
> testCompilation --update-checkfiles
```

Use `--help` to see all the options
```bash
$ sbt
> testCompilation --help
```

### Bootstrapped-only tests

To run `testCompilation` on a bootstrapped Dotty compiler, use
`dotty-compiler-bootstrapped/testCompilation` (with the same syntax as above).
Some tests can only be run in bootstrapped compilers; that includes all tests
with `with-compiler` in their name.

### From TASTy tests

`testCompilation` has a additional mode to run tests that compile code from a `.tasty` file.
 Modify blacklist and whitelists in `compiler/test/dotc` to enable or disable tests from `.tasty` files.

 ```bash
 $ sbt
 > testCompilation --from-tasty
 ```
 
 This mode can be run under `dotty-compiler-bootstrapped/testCompilation` to test on a bootstrapped Dotty compiler.