---
layout: doc-page
title: Command Cheatsheet
---

## sbt commands

Below is a cheat sheet of some frequently used commands to be used from SBT
console – `sbt`.


|                        Command                       |                                                          Description                                                          |
|------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| `scala3/scalac`                                      | Run the compiler directly, with any current changes.                                                                          |
| `scala3/scala`                                       | Run the main method of a given class name.                                                                                    |
| `scalac ../issues/Playground.scala`                  | Compile the given file – path relative to the Dotty directory. Output the compiled class files to the Dotty directory itself. |
| `scala Playground`                                   | Run the compiled class `Playground`. Dotty directory is on classpath by default.                                              |
| `repl`                                               | Start REPL                                                                                                                    |
| `scala3/scalac -print-tasty Foo.tasty`               | Print the TASTy of top-level class `Foo`                                                                                      |
| `scala3-bootstrapped/test`                           | Run all tests for Scala 3. (Slow, recommended for CI only)                                                                    |
| `scala3-bootstrapped/publishLocal`                   | Build Scala 3 locally. (Use to debug a specific project)                                                                      |
| `testOnly dotty.tools.dotc.CompilationTests -- *pos` | Run test (method) `pos` from `CompilationTests` suite.                                                                        |
| `testCompilation sample`                             | In all test suites, run test files containing the word `sample` in their title.                                               |
| `scala3-compiler/Test/runMain dotty.tools.printTypes`| Print types underlying representation                                                                                         |
| `scaladoc/generateScalaDocumentation`                | Build the documentation website (published to https://nightly.scala-lang.org)                                                          |
| `scaladoc/generateReferenceDocumentation`            | Build the reference documentation website (published to https://docs.scala-lang.org/scala3/reference)                         |


## Shell Commands

Below is a cheat sheet of some frequently used commands to be used from your
shell.

| Command                              | Description                                                      |
|--------------------------------------|------------------------------------------------------------------|
| `rm -rv *.tasty *.class out || true` | clean all compiled artifacts, from root dotty directory          |
| `git clean -fdx`                     | a full clean of all files in the codebase not tracked by git     |
