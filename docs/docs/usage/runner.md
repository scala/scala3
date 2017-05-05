---
layout: doc-page
title: "The dotty runner"
---

The dotty runner allows you to locally bootstrap the dotty compiler and
use it to compile and run files and control your cbt builds.

### setup ###

* Follow the instructions for [Cloning and building](contributing/workflow.md)
* Add the `dotty` bash script in dotty's `bin/` directory to your PATH.
* Clone cbt and add the bash script in cbt's root directory to your PATH. See https://github.com/cvogt/cbt/

### compiling files ###

`dotty compile Foo.scala` compiles the file `Foo.scala` in the current directory.

### run files ###

`dotty run Foo.scala <args*>` compiles the file `Foo.scala` and tries to find a class with a main method and invoke it with the given arguments.

### quick / direct ###

`dotty quick <command>`, e.g. `dotty quick run Foo.scala` keeps a background process running and is about 1 second faster per invocation. This is experimental. If you encounter problems try killing the background process.

`dotty direct <command>`, e.g. `dotty direct run Foo.scala` is currently equivalent to not passing either quick or direct and means no background process will be used.

### control your build ###

`dotty build <task>`, for example `dotty build compile` or `dotty build dependencyTree` will treat your local folder as a cbt build and try to interact with it. cbt currently defaults to Scala2 builds if you don't provide a build file. See [Dotty projects with cbt](usage/cbt-projects.md) to learn how to configure a Dotty build.

