---
layout: doc-page
title: Tests for Debuggability
---

## Tools Requires

- JDB
- expect

Both are usually pre-installed on macOS and linux distributions.

## Debug Manually with JDB

First, compile the file `tests/debug/while.scala`:

```shell
$ scalac tests/debug/while.scala
```

Second, run the compiled class with debugging enabled (suppose the main class is `Test`):

```shell
$ scala -d Test
```

Third, start JDB:

```shell
$ jdb -attach 5005 -sourcepath tests/debug/
```

You can run `help` for commands that supported by JDB.

## Debug Automatically with Expect

### 1. Annotate the source code with debug information.

Following file (`tests/debug/while.scala`) is an example of annotated source code:

```Scala
object Test {

  def main(args: Array[String]): Unit = {
    var a = 1 + 2
    a = a + 3
    a = 4 + 5 // [break] [step: while]

    while (a * 8 < 100) { // [step: a += 1]
      a += 1              // [step: while] [cont: print]
    }

    print(a) // [break] [cont]
  }
}
```

The debugging information is annotated as comments to the code in brackets:

```scala
val x = f(3) // [break] [next: line=5]
val y = 5
```

1. A JDB command must be wrapped in brackets, like `[step]`. All JDB commands can be used.
2. To check output of JDB for a command, use `[cmd: expect]`.
3. If `expect` is wrapped in double quotes, regex is supported.
4. Break commands are collected and set globally.
5. Other commands will be send to jdb in the order they appear in the source file

Note that JDB uses line number starts from 1.

### 2. Generate Expect File

Now we can run the following command to generate an expect file:

```shell
compiler/test/debug/Gen tests/debug/while.scala > robot
```

### 3. Run the Test

First, compile the file `tests/debug/while.scala`:

```shell
$ scalac tests/debug/while.scala
```

Second, run the compiled class with debugging enabled:

```shell
$ scala -d Test
```

Finally, run the expect script:

```shell
expect robot
```

## Other Tips

### Adding a New Test

Just put the annotated source file under `tests/debug/`, it will be automatically
run by the test infrastructure.

### Run All Debug Tests

```shell
./compiler/test/debug/test
```

### Debug a Debug Test

If there is any problem with a debug test, first check if the problematic
test work correctly with JDB without automation.

Then, uncomment the following line in the generated expect file to check the
output of expect:

```
# exp_internal 1
```
