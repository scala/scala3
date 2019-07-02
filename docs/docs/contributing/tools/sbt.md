# SBT Commands Cheat Sheet
The basics of working with Dotty codebase are documented [here](http://dotty.epfl.ch/docs/contributing/getting-started.html) and [here](http://dotty.epfl.ch/docs/contributing/workflow.html). Below is a cheat sheet of some frequently used commands (to be used from SBT console – `sbt`).


|                        Command                       |                                                          Description                                                          |
|------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| `dotc ../issues/Playground.scala`                    | Compile the given file – path relative to the Dotty directory. Output the compiled class files to the Dotty directory itself. |
| `dotr Playground`                                    | Run the compiled class `Playground`. Dotty directory is on classpath by default.                                              |
| `repl`                                               | Start REPL                                                                                                                    |
| `testOnly dotty.tools.dotc.CompilationTests -- *pos` | Run test (method) `pos` from `CompilationTests` suite.                                                                        |
| `testCompilation sample`                             | In all test suites, run test files containing the word `sample` in their title.                                               |
