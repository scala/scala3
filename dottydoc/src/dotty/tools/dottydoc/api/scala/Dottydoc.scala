package dotty.tools.dottydoc.api.scala

import dotty.tools.dottydoc.DocDriver
import dotty.tools.dottydoc.model.Package
import dotty.tools.dottydoc.util.OutputWriter

import scala.collection.Map
import java.net.URL

/**
 * The Dottydoc API is fairly simple. The tool creates an index by calling:
 * "createIndex" with the same argument list as you would the compiler - e.g:
 *
 * {{{
 * val array: Array[String] = Array(
 *   "-language:Scala2"
 * )
 *
 * val index: Map[String, Package] = createIndex(array)
 * }}}
 *
 * Once the index has been generated, the tool can also build a documentation
 * API given a Mustache template and a flat resources structure (i.e. absolute
 * paths to each resource, which will be put in the same directory).
 *
 * {{{
 * buildDocs("path/to/output/dir", templateURL, resources, index)
 * }}}
 *
 * The tool can also generate JSON from the created index using "indexToJson"
 * or directly using "createJsonIndex"
 */
trait Dottydoc extends DocDriver {
  /** Creates index from compiler arguments */
  def createIndex(args: Array[String]): Map[String, Package] =
    compiledDocs(args)

  /** Creates JSON from compiler arguments */
  def createJsonIndex(args: Array[String]): String =
    indexToJson(compiledDocs(args))

  /** Creates a documentation from the given parameters */
  def buildDocs(outDir: String, template: URL, resources: List[URL], index: Map[String, Package]) =
    new OutputWriter().write(index, outDir, template, resources)

  /** Writes JSON to an output directory as "index.json" */
  def writeJson(index: Map[String, Package], outputDir: String) =
    new OutputWriter().writeJson(index, outputDir)
}
