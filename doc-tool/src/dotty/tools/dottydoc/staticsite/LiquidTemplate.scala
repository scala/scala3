package dotty.tools
package dottydoc
package staticsite

import dotc.config.Printers.dottydoc

case class LiquidTemplate(contents: String) extends ResourceFinder {
  import scala.collection.JavaConverters._
  import liqp.Template
  import liqp.filters.Filter
  import liqp.parser.Flavor.JEKYLL
  import java.util.{ HashMap, Map => JMap }
  import filters._
  import tags._

  /** Register filters to static container */
  Filter.registerFilter(new Reverse)

  // For some reason, liqp rejects a straight conversion using `.asJava`
  private def toJavaMap(map: Map[String, AnyRef]): HashMap[String, Object] =
    map.foldLeft(new HashMap[String, Object]()) { case (map, (k, v)) =>
      map.put(k, v)
      map
    }

  def render(params: Map[String, AnyRef], includes: Map[String, String]): String =
    Template.parse(contents, JEKYLL)
            .`with`(ResourceInclude(params, includes))
            .`with`(RenderReference(params))
            .render(toJavaMap(params))
}
