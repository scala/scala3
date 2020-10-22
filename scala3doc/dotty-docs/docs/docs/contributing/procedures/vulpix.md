---
layout: doc-page
title: Test Vulpix Framework
---

# Test Vulpix Framework
If you are modifying the Vulpix framework and need a playground with dummy tests to try out your modifications, do the following.

Create the directory structure for the playground:

```bash
mkdir -pv tests/playground/run tests/playground/neg
echo "stuff" > tests/playground/neg/Sample.scala
echo 'object Test { def main(args: Array[String]): Unit = {println("Hi")} }' > tests/playground/run/Sample.scala
```

In `CompilationTests.scala`:

```scala
  @Test def exampleNeg: Unit = {
    implicit val testGroup: TestGroup = TestGroup("exampleNeg")
    compileFilesInDir("tests/playground/neg", defaultOptions).checkExpectedErrors()
  }

  @Test def exampleRun: Unit = {
    implicit val testGroup: TestGroup = TestGroup("exampleRun")
    compileFilesInDir("tests/playground/run", defaultOptions).checkRuns()
  }
```

SBT:

```scala
testOnly dotty.tools.dotc.CompilationTests -- *example*
```
