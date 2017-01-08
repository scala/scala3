package dotty.tools
package dottydoc
package staticsite

import _root_.java.io.{ File => JFile }
import dotc.config.Printers.dottydoc
import dotc.core.Contexts.Context
import scala.io.Source

class Site(val root: JFile) {

  /** If, for some reason, the supplied default files cannot be found - this
    * exception will be thrown in `layouts`.
    */
  final case class ResourceNotFoundException(message: String) extends Exception(message)

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
    def collectLayouts(dir: JFile): Map[String, String] =
      dir
      .listFiles
      .filter(f => f.getName.endsWith(".md") || f.getName.endsWith(".html"))
      .map { f =>
        (f.getName.substring(0, f.getName.lastIndexOf('.')), Source.fromFile(f).mkString)
      }
      .toMap

    val userDefinedLayouts =
      root
      .listFiles.find(d => d.getName == "_layouts" && d.isDirectory)
      .map(collectLayouts)
      .getOrElse(Map.empty)

    val defaultLayouts: Map[String, String] = Map(
      "main" -> "/_layouts/main.html",
      "index" -> "/_layouts/index.html"
    ).mapValues(getResource)

    defaultLayouts ++ userDefinedLayouts
  }

  private def getResource(r: String): String =
    Option(getClass.getResourceAsStream(r)).map(scala.io.Source.fromInputStream)
      .map(_.mkString)
      .getOrElse(throw ResourceNotFoundException(r))

  def render(page: Page, params: Map[String, AnyRef])(implicit ctx: Context): String = {
    page.yaml.get("layout").flatMap(layouts.get(_)) match {
      case None =>
        page.html
      case Some(layout) =>
        val expandedTemplate = new HtmlPage(layout, Map("content" -> page.html) ++ params)
        render(expandedTemplate, params)
    }
  }
}
