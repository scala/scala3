---
layout: doc-page
title: "Worksheet mode with Dotty IDE"
movedTo: https://docs.scala-lang.org/scala3/book/tools-worksheets.html
---

A worksheet is a Scala file that is evaluated on save, and the result of each
expression is shown in a column to the right of your program. Worksheets are
like a REPL session on steroids, and enjoy 1st class editor support: completion,
hyperlinking, interactive errors-as-you-type, etc. Worksheet use the extension
`.sc`.

How to use the worksheets
=========================
The only supported client for the Worksheet mode is [Visual Studio
Code](https://code.visualstudio.com/).

To use the worksheets, start Dotty IDE by [following the
instruction](ide-support.md) and create a new file `MyWorksheet.sc` and
write some code:

```scala
val xyz = 123
println("Hello, worksheets!")
456 + xyz
```

On top of the buffer, the message `Run this worksheet` appears. Click it to
evaluate the code of the worksheet. Each line of output is printed on the right
of the expression that produced it. The worksheets run with the classes of your
project and its dependencies on their classpath.

![](images/worksheets/worksheet-run.png "Run worksheet")

By default, the worksheets are also run when the file is saved. This can be
configured in VSCode preferences:

![](images/worksheets/config-autorun.png "Configure run on save")

Note that the worksheet are fully integrated with the rest of Dotty IDE: While
typing, errors are shown, completions are suggested, and you can use all the
other features of Dotty IDE such as go to definition, find all references, etc.

![](images/worksheets/worksheet-help.png "IDE features in the worksheet")

Implementation details
======================

The implementation details of the worksheet mode and the information necessary to add support for
other clients are available in [Worksheet mode - Implementation
details](worksheet-mode-implementation-details.md).
