---
title: Worksheets
type: section
description: This section looks at worksheets, an alternative to Scala projects.
num: 71
previous-page: tools-sbt
next-page: interacting-with-java
---

A worksheet is a Scala file that is evaluated on save, and the result of each expression is shown
in a column to the right of your program. Worksheets are like a [REPL session] on steroids, and
enjoy 1st class editor support: completion, hyperlinking, interactive errors-as-you-type, etc.
Worksheets use the extension `.worksheet.sc`.

In the following, we show how to use worksheets in IntelliJ, and in VS Code (with the Metals extension).

1. Open a Scala project, or create one.
   - To create a project in IntelliJ, select “File” -> “New” -> “Project…”, select “Scala”
     in the left column, and click “Next” to set the project name and location.
   - To create a project in VS Code, run the command “Metals: New Scala project”, select the
     seed `scala/scala3.g8`, set the project location, open it in a new VS Code window, and
     import its build.
1. Create a file named `hello.worksheet.sc` in the directory `src/main/scala/`.
   - In IntelliJ, right-click on the directory `src/main/scala/`, and select “New”, and
     then “File”.
   - In VS Code, right-click on the directory `src/main/scala/`, and select “New File”.
1. Paste the following content in the editor:
   ~~~
   println("Hello, world!")

   val x = 1
   x + x
   ~~~
1. Evaluate the worksheet.
   - In IntelliJ, click on the green arrow at the top of the editor to evaluate the worksheet.
   - In VS Code, save the file.

   You should see the result of the evaluation of every line on the right panel (IntelliJ), or
   as comments (VS Code).

![]({{ site.baseurl }}/resources/images/scala3-book/intellij-worksheet.png)

A worksheet evaluated in IntelliJ.

![]({{ site.baseurl }}/resources/images/scala3-book/metals-worksheet.png)

A worksheet evaluated in VS Code (with the Metals extension).

Note that the worksheet will use the Scala version defined by your project (set by the key `scalaVersion`,
in your file `build.sbt`, typically).

Also note that worksheets don’t have a [program entry point]. Instead, top-level statements and expressions
are evaluated from top to bottom.

[REPL session]: {% link _overviews/scala3-book/taste-repl.md %}
[program entry point]: {% link _overviews/scala3-book/methods-main-methods.md %}
