package dotty.tools
package dottydoc
package staticsite

import dotc.config.Printers.dottydoc

case class LiquidTemplate(contents: String) extends ResourceFinder {
  import scala.collection.JavaConverters._
  import liqp.{ Template, TemplateContext }
  import liqp.nodes.LNode
  import liqp.tags.Tag
  import liqp.filters.Filter
  import liqp.parser.Flavor.JEKYLL
  import java.util.{ HashMap, Map => JMap }
  import filters._

  /** Register filters to static container */
  Filter.registerFilter(new Reverse)
  Filter.registerFilter(new RenderReference)
  Filter.registerFilter(new RenderLink)

  // For some reason, liqp rejects a straight conversion using `.asJava`
  private def toJavaMap(map: Map[String, AnyRef]): HashMap[String, Object] =
    map.foldLeft(new HashMap[String, Object]()) { case (map, (k, v)) =>
      map.put(k, v)
      map
    }

  def render(params: Map[String, AnyRef], includes: Map[String, String]): String =
    Template.parse(contents, JEKYLL)
            .`with`(ResourceInclude(params, includes))
            .render(toJavaMap(params))

  private case class ResourceInclude(params: Map[String, AnyRef], includes: Map[String, String])
  extends Tag("include") {
    val DefaultExtension = ".html"

    override def render(ctx: TemplateContext, nodes: LNode*): AnyRef = {
      val origInclude = asString(nodes(0).render(ctx))
      val incResource = origInclude match {
        case fileWithExt if fileWithExt.indexOf('.') > 0 => fileWithExt
        case file => file + DefaultExtension
      }

      includes
        .get(incResource)
        .map { template =>
          if (nodes.length > 1) ctx.put(origInclude, nodes(1).render(ctx))
          LiquidTemplate(template).render(Map.empty ++ ctx.getVariables.asScala, includes)
        }
        .getOrElse {
          /*dottydoc.*/println(s"couldn't find include file '$origInclude'")
          ""
        }
    }
  }
}
