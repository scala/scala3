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
- `tests/debug` – these tests are compiled but also debugged. As for `tests/run` they must include at least a `@main def Test = ...`
  See [Debug Tests](#debug-tests).

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
Or for ScalaJS
```bash
$ sbt
> sjsCompilerTests/testOnly -- -Ddotty.tests.updateCheckfiles=TRUE
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

### From TASTy tests

`testCompilation` has an additional mode to run tests that compile code from a `.tasty` file.
Modify the lists in [compiler/test/dotc] to enable or disable tests from `.tasty` files.

```bash
$ sbt
> testCompilation --from-tasty
```

## Debug Tests

Debug tests are a variant of compilation tests located in `compiler/tests/debug`.
Similar to `tests/run`, each test case is executed.
However, instead of verifying the program's output, a debugger is attached to the running program to validate a predefined debug scenario.

The debug scenario is specified in the `.check` file associated with each test case.
It consists of a sequence of debug steps that describe the debugger interactions and outcomes.

**Example debug scenario**:
```
// Pause on a breakpoint in class Test$ on line 5
break Test$ 5

// Stepping in should go to line 10
step 10

// Next should go to line 11
next 11

// Evaluating the expression x should return 42
eval x
result 42
```

To run all the debug tests:
```
sbt 'scala3-compiler/testOnly dotty.tools.debug.DebugTests'
```

### Debug Steps

#### Breakpoint

Syntax:

```
break ${runtime class} ${line number}
```

Examples:

```
break Test$ 5
break example.A 10
break example.A$B$1 12
```

A breakpoint is defined by a fully-qualified class name and a source line.

All breakpoints of a debug scenario are configured before the program starts.

When the program pauses on a breakpoint, we check the class name and source line of the current frame.

### Step in

Syntax:
```
step ${expected line number or method name}
```

Examples:
```
step 10
step println
```

A `step` request expects the program to enter into the called method or go to the next instruction.
After a step request, we check that the source line (or method name) of the current frame matches the expected one.

Typically we use a source line when we stay in the same source file and a method name when we step in a library or JDK class.

### Next

A `next` request behaves similarly to `step` but jumps over a method call and stops on the next instruction.

Syntax:
```
next ${expected line number or method name}
```

Examples:
```
next 10
next println
```

### Evaluation

Syntax:
```
eval ${expression}
result ${expected output}

// or in case an error is expected
eval ${expression}
error ${expected message}
```

It also supports multi-line expressions and multi-line error messages.

Examples:
```
eval fibonacci(2)
result 55

eval
  def square(x: Int): Int =
    x * x
  square(2)
result 4

eval foo
error
  <expression>:1:0
  1 |foo
    |^^^
    | Not found: foo
```

An `eval` request verifies that an expression can be evaluated by the `ExpressionCompiler` during a debugging session.
A `result` assertion checks the evaluation produced the expected output, while an `error` assertion checks the compilation failed with the expected error message.

When the evaluation throws an exception, the exception is returned as a result, not an error.

```
eval throw new Exception("foo")
result java.lang.Exception: foo
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

[CompilationTests]: https://github.com/scala/scala3/blob/master/compiler/test/dotty/tools/dotc/CompilationTests.scala
[compiler/test]: https://github.com/scala/scala3/blob/master/compiler/test/
[compiler/test/dotc]: https://github.com/scala/scala3/tree/master/compiler/test/dotc
[SemanticdbTests]: https://github.com/scala/scala3/blob/master/compiler/test/dotty/tools/dotc/semanticdb/SemanticdbTests.scala
