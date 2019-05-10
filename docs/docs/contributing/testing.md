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

### Testing with checkfiles
#### Idea
Some tests support checking the output of the run or the compilation against a checkfile. A checkfile is a file in which the expected output of the compilation or run is defined. A test against a checkfile fails if the actual output mismatches the expected output.

Currently, only the `neg` (compilation must fail for the test to succeed) and `run` support the checkfiles. `neg`'s checkfiles contain an expected error output during compilation. `run`'s checkfiles contain an expected run output of the successfully compiled program.

Absence of a checkfile is **not** a condition for the test failure. E.g. if a `neg` test fails with the expected number of errors and there is no checkfile for it, the test still passes.

#### Generating checkfiles
If the checkfile is absent, the test framework will automatically generate it from the actual output. This is possible since an absence of the checkfile is equivalent to the presence of the checkfile matching the actual output. The checkfile will be generated only if the actual output is not empty. You can track all the checkfiles created this way by looking for the following output in the console:

```
Created checkfile: /Users/anatolii/Projects/dotty/dotty/tests/generic-java-signatures/primitiveArrayBound.check
Warning: diff check is not performed for out/genericJavaSignatures/generic-java-signatures/primitiveArrayBound since checkfile does not exist. Generating the checkfile now.
```

However, if the checkfile is present and does not match the actual output, the test will fail and the actual output will be written in a file next to the checkfile. You can track these new files by looking for the following console output:

```
Output from 'tests/playground/neg/Sample.scala' did not match check file.
Diff (expected on the left, actual right):
<expected>             |  <actual>

Test output dumped in: /Users/anatolii/Projects/dotty/dotty/tests/playground/neg/Sample.check.out
  See diff of the checkfile
    > diff /Users/anatolii/Projects/dotty/dotty/tests/playground/neg/Sample.check /Users/anatolii/Projects/dotty/dotty/tests/playground/neg/Sample.check.out
  Replace checkfile with current output output
    > mv /Users/anatolii/Projects/dotty/dotty/tests/playground/neg/Sample.check.out /Users/anatolii/Projects/dotty/dotty/tests/playground/neg/Sample.check
```

If you need to replace the checkfile with the actual output, you can do so by running the `mv` command from the above dump.

If you have a lot of false-negatives due to large amount of bad checkfiles and you want to update them all, you can do so by setting the `-Ddotty.tests.updateCheckfiles=TRUE` property. For example:

```scala
testOnly dotty.tools.dotc.CompilationTests -- *example* -Ddotty.tests.updateCheckfiles=TRUE
```

Sometimes you do not want to generate and check against the checkfiles. This requirement most frequently arises when the expected output is non-deterministic. E.g. when you know for sure that the compilation should fail but each time it fails, the error message is different. To ignore checkfiles for a given test file, suffix its name with `_ignore_checkfile`. E.g: `tests/neg/i4385_ignore_checkfile.scala`.

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

`testCompilation` has an additional mode to run tests that compile code from a `.tasty` file.
 Modify blacklist and whitelists in `compiler/test/dotc` to enable or disable tests from `.tasty` files.

 ```bash
 $ sbt
 > testCompilation --from-tasty
 ```
 
 This mode can be run under `dotty-compiler-bootstrapped/testCompilation` to test on a bootstrapped Dotty compiler.
