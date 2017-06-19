# IDE support for Dotty, the experimental Scala compiler

## Prerequisites
To use this in your own Scala project, you must first get it to compile with
Dotty, please follow the instructions at https://github.com/lampepfl/dotty-example-project

## Starting Visual Studio Code from sbt
First, make sure `code`, the binary for Visual Studio Code, is on your `$PATH`,
this is the case if you can start the IDE by running `code` in a terminal. This
is the default on all systems except Mac where you'll need to follow these
instructions: https://code.visualstudio.com/docs/setup/mac#_command-line


If this is the case and your project succesfully compiles with dotty, you can
simply use the `launchIDE` command provided by the sbt-dotty plugin:

```shell
sbt launchIDE
```

## More information

See http://dotty.epfl.ch/docs/usage/ide-support.html
