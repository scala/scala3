package dotty.tools.dottydoc.api.scala

import dotty.tools.dottydoc.DottyDocDriver
import dotty.tools.dottydoc.model.Package
import dotty.tools.dottydoc.util.OutputWriter

import scala.collection.Map

trait Dottydoc extends DottyDocDriver {
  def createIndex(args: Array[String]): Map[String, Package] =
    compiledDocs(args)

  def buildDocs(outDir: String, templatePath: String, resources: List[String], index: Map[String, Package]) =
    new OutputWriter().write(index, templatePath, outDir, resources)
}
