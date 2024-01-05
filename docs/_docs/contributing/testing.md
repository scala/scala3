---
layout: doc-page
title: Testing Your Changes
redirectFrom: /docs/contributing/workflow/testing.html
---

It is important to add tests before a pull request, to verify that everything is working as expected,
and act as proof of what is valid/invalid Scala code (in case it is broken in the future).
In this section you will see the testing procedures in Scala 3.

## Running all Tests

Running all tests in Dotty is as simple as:

```bash
$ sbt test
```
Specifically, `sbt test` runs all tests that do _not_ require a bootstrapped
compiler. In practice, this means that it runs all compilation tests meeting
this criterion, as well as all non-compiler tests.

To run all tests of Scala 3, including for compiler, REPL, libraries and more, run the following in sbt:

```bash
$ sbt
sbt:scala3> scala3-bootstrapped/test
```

Often however it is not necessary to test everything if your changes are localised to one area,
you will see in the following sections the different kinds of tests, and how
to run individual tests.

## Compilation Tests

Compilation tests run the compiler over input files, using various settings. Input files
are found within the `tests/` directory at the root of the compiler repo.

Test input files are categorised further by placing them in the subdirectories
of the `tests/` directory. A small selection of test categories include:

- `tests/pos` – tests that should compile: pass if compiles successfully.
- `tests/neg` – should not compile: pass if fails compilation. Useful, e.g., to test an expected compiler error.
- `tests/run` – these tests not only compile but are also run. Must include at least a `@main def Test = ...`.

### Naming and Running a Test Case

Tests are, by convention, named after the number of the issue they are fixing.
e.g. if you are fixing issue 101, then the test should be named `i101.scala`, for a single-file test,
or be within a directory called `i101/` for a multi-file test.

To run the test, invoke the sbt command `testCompilation i101` (this will match all tests with `"i101"` in
the name, so it is useful to use a unique name)

The test groups – `pos`, `negAll`, etc. – are defined in [CompilationTests]. If you want to run a group
of tests, e.g. `pos`, you can do so via `testOnly *CompilationTests -- *pos` command.

### Testing a Single Input File

If your issue is reproducible by only one file, put that file under an appropriate category.
For example, if your issue is about getting rid of a spurious compiler error (that is a code that doesn't compile should, in fact, compile), you can create a file `tests/pos/i101.scala`.

### Testing Multiple Input Files

If you need more than one file to reproduce an issue, create a directory instead of a file
e.g. `tests/pos/i101/`, and put all the Scala files that are needed to reproduce the issue there.
There are two ways to organise the input files within:

**1: Requiring classpath dependency:** Sometimes issues require one file to be compiled after the other,
(e.g. if the issue only happens with a library dependency, like with Java interop). In this case,
the outputs of the first file compiled will be available to the next file compiled, available via the classpath.
This is called *separate compilation*.

To achieve this, within `tests/pos/i101/`, add a suffix `_n` to each file name, where `n` is an integer defining the
order in which the file will compile. E.g. if you have two files, `Lib.scala` and `Main.scala`, and you need them
compiled separately – Lib first, Main second, then name them `Lib_1.scala` and `Main_2.scala`.

**2: Without classpath dependency:** If your issue does not require a classpath dependency, your files can be compiled
in a single run, this is called *joint compilation*. In this case use file names without the `_n` suffix.

### Checking Program Output

`tests/run` tests verify the run-time behaviour of a test case. The output is checked by invoking a main method
on a class `Test` (which is required to exist even if there is no checkfile), this can be done with either
```scala
@main def Test: Unit = assert(1 > 0)
```
or
```scala
object Test extends scala.App:
  assert(1 > 0)
```

If your program also prints output, this can be compared against `*.check` files.
These contain the expected output of a program. Checkfiles are named after the issue they are checking,
e.g. `tests/run/i101.check` will check either `tests/run/i101.scala` or `tests/run/i101/`.

### Checking Compilation Errors

`tests/neg` tests verify that a file does not compile, and user-facing errors are produced. There are other neg
categories such as `neg-custom-args`, i.e. with `neg` prefixing the directory name. Test files in the `neg*`
categories require annotations for the lines where errors are expected. To do this add one `// error` token to the
end of a line for each expected error. For example, if there are three expected errors, the end of the line should contain
`// error // error // error`.

You can verify the content of the error messages with a `*.check` file. These contain the expected output of the
compiler. Checkfiles are named after the issue they are checking,
e.g. `i101.check` will check either `tests/neg/i101.scala` or `tests/neg/i101/`.
*Note:* checkfiles are not required for the test to pass, however they do add stronger constraints that the errors
are as expected.

### If Checkfiles do not Match Output

If the actual output mismatches the expected output, the test framework will dump the actual output in the file
`*.check.out` and fail the test suite. It will also output the instructions to quickly replace the expected output
with the actual output, in the following format:

```
Test output dumped in: tests/neg/Sample.check.out
  See diff of the checkfile
    > diff tests/neg/Sample.check tests/neg/Sample.check.out
  Replace checkfile with current output
    > mv tests/neg/Sample.check.out tests/neg/Sample.check
```

### Tips for creating Checkfiles

To create a checkfile for a test, you can do one of the following:

1. Create an empty checkfile
   - then add arbitrary content
   - run the test
   - when it fails, use the `mv` command reported by the test to replace the initial checkfile with the actual output.
2. Manually compile the file you are testing with `scala3/scalac`
   - copy-paste whatever console output the compiler produces to the checkfile.

### Automatically Updating Checkfiles

When complex or many checkfiles must be updated, `testCompilation` can run in a mode where it overrides the
checkfiles with the test outputs.
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
`scala3-compiler-bootstrapped/testCompilation` (with the same syntax as above).
Some tests can only be run in bootstrapped compilers; that includes all tests
with `with-compiler` in their name.

### Scala 2 library TASTy tests

Usually we use the Scala 2 library JAR (with classfiles) generated by Scala 2. We can
also use a special version of the library that we generate with only TASTy files. This
JAR is added to the classpath before the Scala 2 library JAR. This allows the compiler
to load the TASTy and the runtime to load the original classfiles.

The library is compiled in `scala2-library-bootstrapped` with TASTy and classfiles.
These classfiles should not be used. The `scala2-library-tasty` project repackages the
JAR `scala2-library-bootstrapped` to only keep TASTy files.

We can enable this library in the build using the SBT setting `useScala2LibraryTasty`. This setting can only be used by bootstrapped compiler tests and is currently only supported for `test` (or `testCompilation`) and `scalac` (or `run`).

```
$ sbt
> set ThisBuild/Build.scala2Library := Build.Scala2LibraryTasty
> scala3-compiler-bootstrapped/scalac MyFile.scala
> scala3-compiler-bootstrapped/test
> scala3-compiler-bootstrapped/testCompilation
```

By default `scala2Library` is set to `Scala2LibraryJar`. This setting can be set to stop using the Scala 2 library TASTy.
```
> set ThisBuild/Build.scala2Library := Build.Scala2LibraryJar
```

#### Scala 2 library with CC TASTy tests
These follow the same structure as the _Scala 2 library TASTy tests_ but add captured checked signatures to the library. The library is compiled in `scala2-library-cc` (instead of `scala2-library-bootstrapped`) and `scala2-library-cc-tasty` (instead of `scala2-library-cc-tasty`).

We can also enable this library in the build using the SBT setting `useScala2LibraryTasty`.
```
> set ThisBuild/Build.scala2Library := Build.Scala2LibraryCCTasty
```


### From TASTy tests

`testCompilation` has an additional mode to run tests that compile code from a `.tasty` file.
Modify the lists in [compiler/test/dotc] to enable or disable tests from `.tasty` files.

```bash
$ sbt
> testCompilation --from-tasty
```

## Unit Tests

Unit tests cover the other areas of the compiler, such as interactions with the REPL, scripting tools and more.
They are defined in [compiler/test], so if your use case isn't covered by this guide,
you may need to consult the codebase. Some common areas are highlighted below:

### SemanticDB tests

To test the SemanticDB output from the `extractSemanticDB` phase (enabled with the `-Xsemanticdb` flag), run the following sbt command:
```bash
$ sbt
sbt:scala3> scala3-compiler-bootstrapped/testOnly
  dotty.tools.dotc.semanticdb.SemanticdbTests
```

[SemanticdbTests] uses source files in `tests/semanticdb/expect` to generate "expect files":
these verify both
- SemanticDB symbol occurrences inline in sourcecode (`*.expect.scala`)
- complete output of all SemanticDB information (`metac.expect`).

Expect files are used as regression tests to detect changes in the compiler.
Their correctness is determined by human inspection.

If expect files change then [SemanticdbTests] will fail, and generate new expect files, providing instructions for
comparing the differences and replacing the outdated expect files.

If you are planning to update the SemanticDB output, you can do it in bulk by running the command
```bash
$ sbt
sbt:scala3> scala3-compiler/Test/runMain
  dotty.tools.dotc.semanticdb.updateExpect
```

then compare the changes via version control.

### Scaladoc tests

See the [Scaladoc contributors guide](./scaladoc.md).

## Troubleshooting

Some of the tests depend on temporary state stored in the `out` directory. In rare cases, that directory
can enter an inconsistent state and cause spurious test failures. If you suspect a spurious test failure,
you can run `rm -rf out/*` from the root of the repository and run your tests again. If that fails, you
can try `git clean -xfd`.

[CompilationTests]: https://github.com/lampepfl/dotty/blob/master/compiler/test/dotty/tools/dotc/CompilationTests.scala
[compiler/test]: https://github.com/lampepfl/dotty/blob/master/compiler/test/
[compiler/test/dotc]: https://github.com/lampepfl/dotty/tree/master/compiler/test/dotc
[SemanticdbTests]: https://github.com/lampepfl/dotty/blob/master/compiler/test/dotty/tools/dotc/semanticdb/SemanticdbTests.scala
