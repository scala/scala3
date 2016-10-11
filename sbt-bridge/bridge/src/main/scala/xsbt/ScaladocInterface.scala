/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.Logger
import dotty.tools.dottydoc.api.scala.Dottydoc
import java.net.URL

class ScaladocInterface {
  def run(args: Array[String], log: Logger, delegate: xsbti.Reporter) =
    (new DottydocRunner(args, log, delegate)).run()
}

class DottydocRunner(args: Array[String], log: Logger, delegate: xsbti.Reporter) extends Dottydoc {
  def run(): Unit = getOutputFolder(args).map { outputFolder =>
    val index     = createIndex(args)
    val resources = getResources(args)
    val template  = getTemplate(resources)

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

  private def getTemplate(resources: List[URL]): Option[URL] =
    resources.find(_.getFile.endsWith("template.html"))

  private def getResources(args: Array[String]): List[URL] = {
    val cp = args sliding (2) find { case Array(x, _) => x == "-classpath" } map (_.tail.head.trim) getOrElse ""

    cp.split(":").find(_.endsWith("dottydoc-client.jar")).map { resourceJar =>
      import java.util.jar.JarFile
      val jarEntries = (new JarFile(resourceJar)).entries
      var entries: List[URL] = Nil

      while (jarEntries.hasMoreElements) {
        val entry = jarEntries.nextElement()

        if (!entry.isDirectory()) {
          val path = s"jar:file:$resourceJar!/${entry.getName}"
          val url  = new URL(path)
          entries = url :: entries
        }
      }

      entries
    } getOrElse (Nil)
  }
}
