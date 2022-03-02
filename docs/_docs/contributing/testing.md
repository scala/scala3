---
layout: doc-page
title: Testing in Dotty
---

Running all tests in Dotty is as simple as:

```bash
$ sbt test
```

Specifically, `sbt test` runs all tests that do _not_ require a bootstrapped
compiler. In practice, this means that it runs all compilation tests meeting
this criterion, as well as all non-compiler tests.

The entire suite of tests can be run using the bootstrapped compiler as follows:

```bash
$ sbt
> scala3-bootstrapped/test
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
> scala3-compiler/test
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

### Testing with checkfiles
Some tests support checking the output of the run or the compilation against a checkfile. A checkfile is a file in which the expected output of the compilation or run is defined. A test against a checkfile fails if the actual output mismatches the expected output.

Currently, the `run` and `neg` (compilation must fail for the test to succeed) tests support the checkfiles. `run`'s checkfiles contain an expected run output of the successfully compiled program. `neg`'s checkfiles contain an expected error output during compilation.

Absence of a checkfile is **not** a condition for the test failure. E.g. if a `neg` test fails with the expected number of errors and there is no checkfile for it, the test still passes.

Checkfiles are located in the same directories as the tests they check, have the same name as these tests with the extension `*.check`. E.g. if you have a test named `tests/neg/foo.scala`, you can create a checkfile for it named `tests/neg/foo.check`. And if you have a test composed of several files in a single directory, e.g. `tests/neg/manyScalaFiles`, the checkfile will be `tests/neg/manyScalaFiles.check`.

If the actual output mismatches the expected output, the test framework will dump the actual output in the file `*.check.out` and fail the test suite. It will also output the instructions to quickly replace the expected output with the actual output, in the following format:

```
Test output dumped in: tests/playground/neg/Sample.check.out
  See diff of the checkfile
    > diff tests/playground/neg/Sample.check tests/playground/neg/Sample.check.out
  Replace checkfile with current output
    > mv tests/playground/neg/Sample.check.out tests/playground/neg/Sample.check
```

To create a checkfile for a test, you can do one of the following:

- Create a dummy checkfile with a random content, run the test, and, when it fails, use the `mv` command reported by the test to replace the dummy checkfile with the actual output.
- Manually compile the file you are testing with `scalac` and copy-paste whatever console output the compiler produces to the checkfile.

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

### Joint and separate sources compilation

When the sources of a test consist of multiple source files places in a single directory they are passed to the compiler in a single run and the compiler decides in which order to compile them. In some cases, however, to reproduce a specific test scenario it might be necessary to compile the source files in several steps in a specified order. To achieve that one can add a `_${step_index}` suffix to a file name (before the `.scala` or `.java` extension) indicating the order of compilation. E.g. if the test directory contains files named `Foo_1.scala`, `Bar_2.scala` and `Baz_2.scala` then `Foo_1.scala` will be compiled first and after that `Bar_2.scala` together with `Baz_2.scala`.

There are also other suffixes indicating how some particular files are compiled:
* `_c${compilerVersion}` - compile a file with a specific version of the compiler instead of the one developed on the current branch
  (e.g. `Foo_c3.0.2.scala`)
* `_r${release}` - compile a file with a given value of `-scala-output-version` flag (e.g. `Foo_r3.0.scala`)

Different suffixes can be mixed together (their order is not important although consistency is advised), e.g. `Foo_1_r3.0`, `Bar_2_c3.0.2`.

### Bootstrapped-only tests

To run `testCompilation` on a bootstrapped Dotty compiler, use
`scala3-compiler-bootstrapped/testCompilation` (with the same syntax as above).
Some tests can only be run in bootstrapped compilers; that includes all tests
with `with-compiler` in their name.

### From TASTy tests

`testCompilation` has an additional mode to run tests that compile code from a `.tasty` file.
 Modify blacklist and whitelists in `compiler/test/dotc` to enable or disable tests from `.tasty` files.

 ```bash
 $ sbt
 > testCompilation --from-tasty
 ```

 This mode can be run under `scala3-compiler-bootstrapped/testCompilation` to test on a bootstrapped Dotty compiler.

### SemanticDB tests

```bash
$ sbt
> scala3-compiler-bootstrapped/testOnly dotty.tools.dotc.semanticdb.SemanticdbTests
```

The output of the `extractSemanticDB` phase, enabled with `-Xsemanticdb` is tested with the bootstrapped JUnit test
`dotty.tools.dotc.semanticdb.SemanticdbTests`. It uses source files in `tests/semanticdb/expect` to generate
two kinds of output file that are compared with "expect files": placement of semanticdb symbol occurrences inline in
sourcecode (`*.expect.scala`), for human verification by inspection; and secondly metap formatted output which outputs
all information stored in semanticdb (`metac.expect`).
Expect files are used as regression tests to detect changes in the compiler.

The test suite will create a new file if it detects any difference, which can be compared with the
original expect file, or if the user wants to globally replace all expect files for semanticdb they can use
`scala3-compiler-bootstrapped/test:runMain dotty.tools.dotc.semanticdb.updateExpect`, and compare the changes via version
control.

## Troubleshooting

Some of the tests depend on temporary state stored in the `out` directory. In rare cases, that directory
can enter an inconsistent state and cause spurious test failures. If you suspect a spurious test failure,
you can run `rm -rf out/*` from the root of the repository and run your tests again. If that fails, you
can try `git clean -xfd`.
