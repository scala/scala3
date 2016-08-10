/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.Logger
import dotty.tools.dottydoc.api.scala.Dottydoc

class ScaladocInterface {
  def run(args: Array[String], log: Logger, delegate: xsbti.Reporter) =
    (new DottydocRunner(args, log, delegate)).run()
}

class DottydocRunner(args: Array[String], log: Logger, delegate: xsbti.Reporter) extends Dottydoc {
  def run(): Unit = {
    println(args.mkString("Args = List (\n  ",",\n  ",")"))
    //val index = createIndex(args)
    //buildDocs(outputDir, templatePath, resources, index)
  }
}
