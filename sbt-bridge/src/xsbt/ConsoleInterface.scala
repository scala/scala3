/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.Logger

import dotty.tools.dotc.core.Contexts.Context
// TODO: decide what to do with sbt REPL interface
// import dotty.tools.dotc.repl.REPL
// import dotty.tools.dotc.repl.REPL.Config

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
    //val completeArgs =
    //  args                                    :+
    //  "-bootclasspath" :+ bootClasspathString :+
    //  "-classpath"     :+ classpathString

    //println("Starting dotty interpreter...")
    //val repl = ConsoleInterface.customRepl(
    //  initialCommands :: Nil,
    //  cleanupCommands :: Nil,
    //  bindNames zip bindValues,
    //  loader
    //)
    //repl.process(completeArgs)
  }
}

object ConsoleInterface {
  //def customConfig(
  //  initCmds: List[String],
  //  cleanupCmds: List[String],
  //  boundVals: Array[(String, Any)],
  //  loader: ClassLoader
  //) = new Config {
  //  override val initialCommands: List[String] = initCmds
  //  override val cleanupCommands: List[String] = cleanupCmds
  //  override val boundValues: Array[(String, Any)] = boundVals
  //  override val classLoader: Option[ClassLoader] = Option(loader)
  //}

  //def customRepl(cfg: Config): REPL = new REPL {
  //  override lazy val config = cfg
  //}

  //def customRepl(
  //  initCmds: List[String],
  //  cleanupCmds: List[String],
  //  boundVals: Array[(String, Any)],
  //  loader: ClassLoader
  //): REPL = customRepl(customConfig(initCmds, cleanupCmds, boundVals, loader))
}
