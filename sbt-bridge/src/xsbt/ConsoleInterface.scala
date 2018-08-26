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

    val driver = new ReplDriver(completeArgs, classLoader = Some(loader))

    val s0 = (bindNames, bindValues).zipped.foldLeft(driver.initialState) {
      case (state, (name, value)) => driver.bind(name, value)(state)
    }

    val s1 = driver.run(initialCommands)(s0)
    // TODO handle failure during initialisation
    val s2 = driver.runUntilQuit(s1)
    driver.run(cleanupCommands)(s2)
  }
}
