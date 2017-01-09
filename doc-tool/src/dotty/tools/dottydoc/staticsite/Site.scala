package dotty.tools
package dottydoc
package staticsite

import _root_.java.io.{ File => JFile }
import dotc.config.Printers.dottydoc
import dotc.core.Contexts.Context
import scala.io.Source

class Site(val root: JFile) extends ResourceFinder {

  /** Files that define a layout then referred to by `layout: filename-no-ext`
    * in yaml front-matter.
    *
    * The compiler will look in two locations, `<root>/_layouts/` and
    * in the bundled jar file's resources `/_layouts`.
    *
    * If the user supplies a layout that has the same name as one of the
    * defaults, the user-defined one will take precedence.
    */
  val layouts: Map[String, String] = {
    val userDefinedLayouts =
      root
      .listFiles.find(d => d.getName == "_layouts" && d.isDirectory)
      .map(collectFiles(_, f => f.endsWith(".md") || f.endsWith(".html")))
      .getOrElse(Map.empty)

    val defaultLayouts: Map[String, String] = Map(
      "main" -> "/_layouts/main.html",
      "index" -> "/_layouts/index.html"
    ).mapValues(getResource)

    defaultLayouts ++ userDefinedLayouts
  }

  val includes: Map[String, String] = {
    val userDefinedIncludes =
      root
      .listFiles.find(d => d.getName == "_includes" && d.isDirectory)
      .map(collectFiles(_, f => f.endsWith(".md") || f.endsWith(".html")))
      .getOrElse(Map.empty)

    val defaultIncludes: Map[String, String] = Map(
      "header.html" -> "/_includes/header.html"
    ).mapValues(getResource)

    defaultIncludes ++ userDefinedIncludes
  }

  private def collectFiles(dir: JFile, includes: String => Boolean): Map[String, String] =
    dir
    .listFiles
    .filter(f => includes(f.getName))
    .map { f =>
      (f.getName.substring(0, f.getName.lastIndexOf('.')), Source.fromFile(f).mkString)
    }
    .toMap

  def render(page: Page, params: Map[String, AnyRef])(implicit ctx: Context): String = {
    page.yaml.get("layout").flatMap(layouts.get(_)) match {
      case None =>
        page.html
      case Some(layout) =>
        val newParams = Map("content" -> page.html) ++ params ++ Map("page" -> page.yaml)
        val expandedTemplate = new HtmlPage(layout, newParams, includes)
        render(expandedTemplate, params)
    }
  }
}
