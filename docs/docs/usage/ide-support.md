---
layout: doc-page
title: "IDE support for Scala 3"
movedTo: https://docs.scala-lang.org/scala3/getting-started.html
---

IDE support for Scala 3 is available in IDEs based on [Scala Metals](https://scalameta.org/metals/)
(e.g., Visual Studio Code, vim) and in [IntelliJ IDEA](https://www.jetbrains.com/idea/).


## Using Visual Studio Code

To use Visual Studio Code on a Scala 3 project, ensure you have the
[Metals](https://scalameta.org/metals/docs/editors/vscode.html) plugin installed. Then open the
project directory in VS code and click the "Import build" button in notification.


### Under the Hood

VS Code implements semantic features (such as completions, "go to definition")
using the [Language Server Protocol (LSP)](https://github.com/Microsoft/language-server-protocol),
so it needs a language server implementation. Metals is the implementation of LSP for Scala. It
extracts semantic information from [semanticdb](https://scalameta.org/docs/semanticdb/guide.html),
which is generated directly by the Scala 3 compiler.

You can read more about Scala 3 support in Metals in
[this blog post](https://medium.com/virtuslab/introduction-to-metals-with-scala-3-79ebf3120a95).

To communicate with the build tool (e.g., to import the project, trigger builds, run tests),
Metals uses the [Build Server Protocol (BSP)](https://build-server-protocol.github.io/). The
default BSP implementation used by metals is [Bloop](https://scalacenter.github.io/bloop/), which
supports Scala 3 projects. Alternatively,
[sbt can be used as a BSP server](https://scalameta.org/metals/blog/2020/11/06/sbt-BSP-support.html)
as it directly implements BSP since version 1.4.


## Using IntelliJ IDEA

IntelliJ has its own implementation for semantic features, so it does not use Metals or the
Language Server Protocol (LSP).

In order to import a project into IntelliJ there are two possibilities:

  - Use the built-in feature to import sbt builds
  - Use IntelliJ's support for the
    [Build Server Protocol (BSP)](https://www.jetbrains.com/help/idea/bsp-support.html)


### Importing the sbt build

To use IntelliJ's sbt import, go to "File" - "Open..." and select your project's `build.sbt` file.

In this mode, IntelliJ starts sbt with a custom plugin to extract the project structure. After
importing, IntelliJ no longer interacts with other sbt sessions. Building and running the project
within the IDE is done by separate processes.


### Importing the project using BSP

To import a project using BSP, go to "File" - "New" - "Project from Existing Sources" and select
the project directory. In the upcoming dialog select "BSP" to import the project. You may be asked
to choose between "sbt" and "sbt with Bloop", the recommended option is "sbt".

If the project import fails ("Problem executing BSP job"), navigate to your project in a terminal
and just start `sbt`. Once sbt is running, open the "bsp" tab in IntelliJ click the "Reload" button.

When using IntelliJ's BSP mode, build and run commands from the IDE are executed through sbt, so
they have the same effect as building or running the project through sbt in the terminal.
