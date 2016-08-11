package dotty.tools.dottydoc.api.scala

import dotty.tools.dottydoc.DocDriver
import dotty.tools.dottydoc.model.Package
import dotty.tools.dottydoc.util.OutputWriter

import scala.collection.Map

/** FIXME: document this class plz */
trait Dottydoc extends DocDriver {
  def createIndex(args: Array[String]): Map[String, Package] =
    compiledDocs(args)

  def buildDocs(outDir: String, templatePath: String, resources: List[String], index: Map[String, Package]) =
    new OutputWriter().write(index, templatePath, outDir, resources)

  def writeJson(index: Map[String, Package], outputDir: String) =
    new OutputWriter().writeJson(index, outputDir)
}
