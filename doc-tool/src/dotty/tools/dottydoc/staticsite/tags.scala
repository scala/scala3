package dotty.tools
package dottydoc
package staticsite

import model.references._

import liqp.tags.Tag
import liqp.TemplateContext
import liqp.nodes.LNode

import java.util.{ Map => JMap }

object tags {

  sealed trait ParamConverter {
    def params: Map[String, AnyRef]

    val baseurl: String =
      params.get("site").flatMap {
        case map: JMap[String, String] @unchecked =>
          Some(map.get("baseurl"))
        case _ =>
          None
      }
      .getOrElse {
        /*dottydoc.*/println(s"missing `baseurl` in: $params")
        ""
      }
  }

  /** Renders a `MaterializableLink` into a HTML anchor tag. If the link is
    * `NoLink` it will just return a string with the link's title.
    */
  final case class RenderLink(params: Map[String, AnyRef]) extends Tag("renderLink") with ParamConverter {
    override def render(ctx: TemplateContext, nodes: LNode*): AnyRef = nodes(0).render(ctx) match {
      case map: JMap[String, AnyRef] @unchecked =>
        val link = map.get("scala")
        if (link.isInstanceOf[MaterializableLink] && (link ne null))
          renderLink(baseurl, link.asInstanceOf[MaterializableLink])
        else if (link eq null)
          null // Option[Reference] was None
        else {
          /*dottydoc.*/println(s"illegal argument: $link, to `renderLink` function")
          null
        }
      case _ => null
    }
  }


  private[this] def renderLink(baseurl: String, link: MaterializableLink): String = link match {
    case MaterializedLink(title, target) =>
      s"""<a href="$baseurl/api/$target">$title</a>"""
    case _ => link.title
  }

  final case class RenderReference(params: Map[String, AnyRef])
  extends Tag("renderRef") with ParamConverter {

    private def renderReference(ref: Reference): String = ref match {
      case TypeReference(_, tpeLink, paramLinks) => {
        if (paramLinks.nonEmpty) {
          s"""|${renderLink(baseurl, tpeLink)}
              |<span class="no-left no-right">[</span>
              |${ paramLinks.map(renderReference).mkString("""<span class="">, </span>""") }
              |<span class="no-left">]</span>""".stripMargin
        }
        else renderLink(baseurl, tpeLink)
      }

      case AndOrTypeReference(left, sep, right) =>
        s"""${renderReference(left)}<span class="and-or-separator"> $sep </span>${renderReference(right)}"""

      case FunctionReference(args, returnValue) => {
        val params =
          if (args.isEmpty) "<span>() =&gt; </span>"
          else if (args.tail.isEmpty) renderReference(args.head) + """<span class="right-arrow"> =&gt; </span>"""
          else args.map(renderReference).mkString("<span>(</span>", "<span>, </span>", "<span>) =&gt; </span>")

        params + renderReference(returnValue)
      }

      case TupleReference(args) =>
        s"""|<span class="no-right">(</span>
            |${ args.map(renderReference).mkString("<span>, </span>") }
            |<span class="no-left">)</span>""".stripMargin

      case BoundsReference(low, high) =>
        s"""${ renderReference(low) }<span class="bounds"> &lt;: </span>${ renderReference(high) }"""

      case NamedReference(title, _, _, _) =>
        /*dottydoc.*/println(s"received illegal named reference in rendering: $ref")
        title

      case ConstantReference(title) => title
    }
    override def render(ctx: TemplateContext, nodes: LNode*): AnyRef = nodes(0).render(ctx) match {
      case map: JMap[String, AnyRef] @unchecked =>
        val ref = map.get("scala")
        if (ref.isInstanceOf[Reference] && (ref ne null)) renderReference(ref.asInstanceOf[Reference])
        else if (ref eq null) null // Option[Reference] was None
        else {
          /*dottydoc.*/println(s"illegal argument: $ref, to `renderRef` function")
          null
        }
      case _ => null
    }
  }

  case class ResourceInclude(params: Map[String, AnyRef], includes: Map[String, String])
  extends Tag("include") {
    import scala.collection.JavaConverters._
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
