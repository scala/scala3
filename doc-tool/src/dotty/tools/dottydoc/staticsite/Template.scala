package dotty.tools
package dottydoc
package staticsite

import scala.util.control.NonFatal
import dotc.util.SourceFile

trait Template {
  def path: String
  def content: SourceFile
  def show: String = new String(content.content)
}

case class TemplateRenderingError(path: String, ex: Throwable)
extends Exception(s"error rendering $path, $ex")

case class Layout(path: String, content: SourceFile) extends Template

case class Include(path: String, content: SourceFile) extends Template

case class LiquidTemplate(path: String, content: String) extends ResourceFinder {
  import scala.collection.JavaConverters._
  import liqp.Template
  import liqp.filters.Filter
  import liqp.parser.Flavor.JEKYLL
  import java.util.{ HashMap, Map => JMap }
  import filters._
  import tags._

  /** Register filters to static container */
  Filter.registerFilter(new Reverse)
  Filter.registerFilter(new First)

  // For some reason, liqp rejects a straight conversion using `.asJava`
  private def toJavaMap(map: Map[String, AnyRef]): HashMap[String, Object] =
    map.foldLeft(new HashMap[String, Object]()) { case (map, (k, v)) =>
      map.put(k, v)
      map
    }

  def render(params: Map[String, AnyRef], includes: Map[String, Include]): String =
    try {
      Template.parse(content, JEKYLL)
        .`with`(ResourceInclude(params, includes))
        .`with`(RenderReference(params))
        .`with`(RenderTitle(params))
        .`with`(Docstring(params))
        .render(toJavaMap(params))
    } catch {
      case NonFatal(ex) =>
        throw TemplateRenderingError(path, ex)
    }
}
