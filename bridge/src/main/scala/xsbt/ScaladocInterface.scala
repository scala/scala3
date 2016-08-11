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
  } getOrElse {
    delegate.log(
      NoPosition,
      "No output folder set for API documentation (\"-d\" parameter should be passed to the documentation tool)",
      xsbti.Severity.Error
    )
  }

  private[this] val NoPosition = new xsbti.Position {
    val line = xsbti.Maybe.nothing[Integer]
    val lineContent = ""
    val offset = xsbti.Maybe.nothing[Integer]
    val sourcePath = xsbti.Maybe.nothing[String]
    val sourceFile = xsbti.Maybe.nothing[java.io.File]
    val pointer = xsbti.Maybe.nothing[Integer]
    val pointerSpace = xsbti.Maybe.nothing[String]
  }

  private def getStringSetting(name: String): Option[String] =
    args find (_.startsWith(name)) map (_.drop(name.length))

  private def getOutputFolder(args: Array[String]): Option[String] =
    args sliding(2) find { case Array(x, _) => x == "-d" } map (_.tail.head.trim)

  private def getTemplate(args: Array[String]): Option[String] =
    getStringSetting("-template:")

  private def getResources(args: Array[String]): List[String] =
    getStringSetting("-resources:").map { path =>
      val dir = new java.io.File(path)
      if (dir.exists && dir.isDirectory)
        dir.listFiles.filter(_.isFile).map(_.getAbsolutePath).toList
      else Nil
    }.getOrElse(Nil)

}
