/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.Logger

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.repl.ReplDriver

class ConsoleInterface {
  def commandArguments(
    args: Array[String],
    bootClasspathString: String,
    classpathString: String,
    log: Logger
  ): Array[String] = args

  def run(args: Array[String],
          bootClasspathString: String,
          classpathString: String,
          // TODO: initial commands needs to be run under some form of special
          // "silent" mode in the REPL. I.e. the effects should be had without
          // any visual output.
          //
          // To do this we can use the `run` interface to the `ReplDriver` and
          // pass it a special instance of `ParseResult` like `Silently(res: ParseResult)`
          // and then observe the effects without printing to `ReplDriver#out`
          //
          // This way, the REPL can offer feedback on invalid commands but
          // still function without stringly logic.
          //
          // This same principle can be applied to `cleanupCommands` and
          // `bindValues`
          //
          // Steps:
          //
          // 1. Introduce `case class Silent(res: ParseResult) extends ParseResult`
          // 2. Perform all steps in `interpret` as usual without printing to `out`
          initialCommands: String,
          cleanupCommands: String,
          loader: ClassLoader,
          bindNames: Array[String],
          bindValues: Array[Any],
          log: Logger
  ): Unit = {
    val completeArgs =
      args ++ {
        if (bootClasspathString.isEmpty) Array.empty[String]
        else Array("-bootclasspath", bootClasspathString)
      } ++
      Array("-classpath", classpathString)

    new ReplDriver(completeArgs, classLoader = Some(loader)).runUntilQuit()
  }
}
