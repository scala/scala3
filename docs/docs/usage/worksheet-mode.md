---
layout: doc-page
title: "Worksheet mode with Metals"
---

A worksheet is a Scala file that is evaluated on save, powered
by [Scala Metals](https://scalameta.org/metals/), and the result of each
expression is shown at the end of the line it is defined on. Worksheets are
like a REPL session on steroids, and enjoy 1st class editor support: completion,
hyperlinking, interactive errors-as-you-type, etc. Worksheet use the extension
`.worksheet.sc`.

How to use the worksheets
=========================
The only supported client for the Worksheet mode is [Visual Studio
Code](https://code.visualstudio.com/).

To use the worksheets, start Metals by [following the
instruction](ide-support.md) and create a new file `MyWorksheet.worksheet.sc` and
write some code:

```scala
val xyz = 123
println("Hello, worksheets!")
456 + xyz
```

On top of the buffer, the message `Copy Worksheet Output` appears. Save the file to evaluate the code of the worksheet. Each line of output is printed on the right
of the expression that produced it. The worksheets run with the classes of your
project and its dependencies on their classpath.

![](images/worksheets/worksheet-run.png "Run worksheet")

Note that the worksheet are fully integrated with the rest of Metals:
When you type, completions are suggested, and when you save errors are shown.
You can use all the other features of Metals such as go to definition,
find all references, etc.

![](images/worksheets/worksheet-help.png "IDE features in the worksheet")

More Information
======================

find out more in the [Metals Documentation](https://scalameta.org/metals/docs/editors/vscode.html#worksheets).
