---
layout: doc-page
title: "IDE support for Dotty"
---

Dotty comes built-in with the Dotty Language Server, an implementation of the
[Language Server Protocol](https://github.com/Microsoft/language-server-protocol),
which means that any editor that implements the LSP can be used as a Dotty IDE.
Currently, the only IDE we officially support is
[Visual Studio Code](https://code.visualstudio.com/).

Prerequisites
============
To use this in your own Scala project, you must first get it to compile with
Dotty, please follow the instructions at https://github.com/lampepfl/dotty-example-project

Usage
=====
1. Install [Visual Studio Code](https://code.visualstudio.com/).
2. Make sure `code`, the binary for Visual Studio Code, is on your `$PATH`, this
   is the case if you can start the IDE by running `code` in a terminal. This
   is the default on all systems except Mac where you'll need to follow these
   instructions: https://code.visualstudio.com/docs/setup/mac#_command-line
3. In your project, run:
```shell
sbt launchIDE
```

Status
======

## Fully supported features:
- Typechecking as you type to show compiler errors/warnings
- Type information on hover
- Go to definition (in the current project)
- Find all references
- Documentation on hover
- [Worksheet mode](worksheet-mode.md)

## Partially working features:
- Completion
- Renaming
- Go to definition in external projects

## Unimplemented features:
- Formatting code (requires integrating with scalafmt)
- Quick fixes (probably by integrating with scalafix)

## Current limitations, to be fixed:
- Projects should be compiled with sbt before starting the IDE, this is
  automatically done for you if you run `sbt launchIDE`.
- Once the IDE is started, source files that are not opened in the IDE
  should not be modified in some other editor, the IDE won't pick up
  these changes.
- Not all compiler errors/warnings are displayed, just those occurring
  during typechecking.


Feedback
========
Please report issues on https://github.com/lampepfl/dotty/issues,
you can also come chat with use on the
[Dotty gitter channel](https://gitter.im/lampepfl/dotty)!
