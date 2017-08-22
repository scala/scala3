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
