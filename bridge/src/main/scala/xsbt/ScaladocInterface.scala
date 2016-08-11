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
  def run(): Unit = getOutputFolder(args).map { outputFolder =>
    val index     = createIndex(args)
    val template  = getTemplate(args)
    val resources = getResources(args)

    template.fold(writeJson(index, outputFolder)) { tpl =>
      buildDocs(outputFolder, tpl, resources, index)
    }
  }

  private def getOutputFolder(args: Array[String]): Option[String] =
    args sliding(2) find { case Array(x, _) => x == "-d" } map (_.tail.head)

  private def getTemplate(args: Array[String]): Option[String] = None

  private def getResources(args: Array[String]): List[String] = Nil
}
