package dotty.tools
package dottydoc

import scala.io.Source

object WhitelistedStandardLib extends dottydoc.java.Dottydoc {
  import scala.collection.JavaConverters._

  val files: List[String] = {
    val whitelist = "../test/dotc/scala-collections.whitelist"

    Source.fromFile(whitelist, "UTF8")
      .getLines()
      .map(_.trim) // allow identation
      .filter(!_.startsWith("#")) // allow comment lines prefixed by #
      .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
      .filter(_.nonEmpty)
      .filterNot(_.endsWith("package.scala"))
      .map("." + _)
      .toList
  }

  private val resources = List(
    "../js-dottydoc/resources/MaterialIcons-Regular.eot",
    "../js-dottydoc/resources/MaterialIcons-Regular.ijmap",
    "../js-dottydoc/resources/MaterialIcons-Regular.svg",
    "../js-dottydoc/resources/MaterialIcons-Regular.ttf",
    "../js-dottydoc/resources/MaterialIcons-Regular.woff",
    "../js-dottydoc/resources/MaterialIcons-Regular.woff2",
    "../js-dottydoc/resources/codepoints",
    "../js-dottydoc/resources/github.css",
    "../js-dottydoc/resources/highlight.pack.js",
    "../js-dottydoc/resources/index.css",
    "../js-dottydoc/resources/material-icons.css",
    "../js-dottydoc/resources/material.min.css",
    "../js-dottydoc/resources/material.min.js"/*,
    "resources/dottydoc-fastopt.js"*/
  )

  override def main(args: Array[String]) = {
    val compilerArgs =
      "-language:Scala2" +: "-Ydoc-output" +: "../build/dottydoc" +: files.toArray

    val index = createIndex(compilerArgs)
    buildDocs("../build/dottydoc", "../js-dottydoc/template.html", resources.asJava, index)
  }
}
