package dotty.tools
package dottydoc

import scala.io.Source

object WhitelistedStandardLib extends dottydoc.api.java.Dottydoc {
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
    "../../dottydoc-client/resources/MaterialIcons-Regular.eot",
    "../../dottydoc-client/resources/MaterialIcons-Regular.ijmap",
    "../../dottydoc-client/resources/MaterialIcons-Regular.svg",
    "../../dottydoc-client/resources/MaterialIcons-Regular.ttf",
    "../../dottydoc-client/resources/MaterialIcons-Regular.woff",
    "../../dottydoc-client/resources/MaterialIcons-Regular.woff2",
    "../../dottydoc-client/resources/codepoints",
    "../../dottydoc-client/resources/github.css",
    "../../dottydoc-client/resources/highlight.pack.js",
    "../../dottydoc-client/resources/index.css",
    "../../dottydoc-client/resources/material-icons.css",
    "../../dottydoc-client/resources/material.min.css",
    "../../dottydoc-client/resources/material.min.js",
    "../../dottydoc-client/target/scala-2.11/dottydoc-client-fastopt.js"
  )

  override def main(args: Array[String]) = {
    val compilerArgs =
      "-language:Scala2" +: "-Ydoc-output" +: "../build/dottydoc" +: files.toArray

    val index = createIndex(compilerArgs)
    buildDocs("../build/dottydoc", "../../dottydoc-client/resources/template.html", resources.asJava, index)
  }
}
