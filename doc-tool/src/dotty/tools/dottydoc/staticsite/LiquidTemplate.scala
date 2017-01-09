package dotty.tools
package dottydoc
package staticsite

import dotc.config.Printers.dottydoc

case class LiquidTemplate(contents: String) extends ResourceFinder {
  import scala.collection.JavaConverters._
  import liqp.{ Template, TemplateContext }
  import liqp.nodes.LNode
  import liqp.tags.Tag

  def render(params: Map[String, AnyRef], includes: Map[String, String]): String = {
    Template.parse(contents).`with`(ResourceInclude(params, includes)).render(params.asJava)
  }

  private case class ResourceInclude(params: Map[String, AnyRef], includes: Map[String, String])
  extends Tag("include") {
    val DefaultExtension = ".html"

    private def renderTemplate(template: String) = "dude"

    override def render(ctx: TemplateContext, nodes: LNode*): AnyRef = {
      val origInclude = asString(nodes(0).render(ctx))
      val incResource = origInclude match {
        case fileWithExt if fileWithExt.indexOf('.') > 0 => fileWithExt
        case file => file + DefaultExtension
      }

      includes
        .get(incResource)
        .map { template =>
          val additionalParams =
            // include has `with` clause:
            if (nodes.length > 1) params + (origInclude -> nodes(1).render(ctx))
            else params

          Template.parse(template, ctx.flavor).render(additionalParams.asJava)
        }
        .getOrElse {
          /*dottydoc.*/println(s"couldn't find include file '$origInclude'")
          ""
        }
    }
  }
}
