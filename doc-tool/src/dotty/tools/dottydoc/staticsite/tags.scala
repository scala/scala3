package dotty.tools
package dottydoc
package staticsite

import model.references._
import dotc.core.Contexts.Context

import liqp.tags.Tag
import liqp.TemplateContext
import liqp.nodes.LNode

import java.util.{ Map => JMap }
import model._
import util.syntax._

object tags {

  sealed trait ParamConverter {
    def params: Map[String, AnyRef]

    private[this] var _baseurl: String = _
    def baseurl(implicit ctx: Context): String = {
      if (_baseurl eq null) {
        _baseurl =
          params.get("site").flatMap {
            case map: JMap[String, String] @unchecked =>
              Some(map.get("baseurl"))
            case _ =>
              None
          }
          .getOrElse {
            ctx.docbase.warn(s"missing `baseurl` in: $params")
            ""
          }
      }
      _baseurl
    }
  }

  /** Renders a `MaterializableLink` into a HTML anchor tag. If the link is
    * `NoLink` it will just return a string with the link's title.
    */
  final case class RenderLink(params: Map[String, AnyRef])(implicit ctx: Context)
  extends Tag("renderLink") with ParamConverter {
    override def render(tctx: TemplateContext, nodes: LNode*): AnyRef = nodes(0).render(tctx) match {
      case map: JMap[String, AnyRef] @unchecked =>
        val link = map.get("scala")
        if (link.isInstanceOf[MaterializableLink] && (link ne null))
          renderLink(baseurl, link.asInstanceOf[MaterializableLink])
        else if (link eq null)
          null // Option[Reference] was None
        else {
          ctx.docbase.error(s"illegal argument: $link, to `renderLink` function")
          null
        }
      case _ => null
    }
  }


  private[this] def renderLink(baseurl: String, link: MaterializableLink)(implicit ctx: Context): String =
    link match {
      case MaterializedLink(title, target) =>
        s"""<a href="$baseurl/api/$target">$title</a>"""
      case _ => link.title
    }

  final case class RenderReference(params: Map[String, AnyRef])(implicit ctx: Context)
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
        ctx.docbase.error(s"received illegal named reference in rendering: $ref")
        title

      case ConstantReference(title) => title

      case _ =>  // EmptyReference
        ctx.docbase.error(s"invalid reference: $ref")
        null
    }
    override def render(tctx: TemplateContext, nodes: LNode*): AnyRef = nodes(0).render(tctx) match {
      case map: JMap[String, AnyRef] @unchecked =>
        val ref = map.get("scala")
        if (ref.isInstanceOf[Reference] && (ref ne null)) renderReference(ref.asInstanceOf[Reference])
        else if (ref eq null) null // Option[Reference] was None
        else {
          ctx.docbase.error(s"illegal argument: $ref, to `renderRef` function")
          null
        }
      case _ => null
    }
  }

  case class ResourceInclude(params: Map[String, AnyRef], includes: Map[String, Include])(implicit ctx: Context)
  extends Tag("include") {
    import scala.collection.JavaConverters._
    val DefaultExtension = ".html"

    override def render(tctx: TemplateContext, nodes: LNode*): AnyRef = {
      val origInclude = asString(nodes(0).render(tctx))
      val incResource = origInclude match {
        case fileWithExt if fileWithExt.indexOf('.') > 0 => fileWithExt
        case file => file + DefaultExtension
      }

      includes
        .get(incResource)
        .map { template =>
          if (nodes.length > 1) tctx.put(origInclude, nodes(1).render(tctx))

          LiquidTemplate(template.path, template.content)
            .render(tctx.getVariables.asScala.toMap, includes)
            .getOrElse("")
        }
        .getOrElse {
          ctx.docbase.error(s"couldn't find include file '$origInclude'")
          ""
        }
    }
  }

  /** Can be used to render the `sidebar.yml` entries, represented here as
    * `Title`.
    *
    * ```html
    * {% renderTitle title, parent %}
    * ```
    *
    * The rendering currently works on depths up to 2. This means that each
    * title can have a subsection with its own titles.
    */
  case class RenderTitle(params: Map[String, AnyRef])(implicit ctx: Context)
  extends Tag("renderTitle") with ParamConverter {
    private def renderTitle(t: Title, parent: String): String = {
        if (!t.url.isDefined && t.subsection.nonEmpty) {
          val onclickFunction =
            s"""(function(){var child=document.getElementById("${t.title}");child.classList.toggle("show");child.classList.toggle("hide");})();"""
          s"""|<a class="toggle-children" onclick='$onclickFunction'>${t.title}</a>
              |<ul id="${t.title}" class="${if (parent.toLowerCase == t.title.toLowerCase) "show" else "hide"}">
              |    ${t.subsection.map(renderTitle(_, parent)).mkString("<li>", "</li><li>", "</li>")}
              |</ul>""".stripMargin
        }
        else if (t.url.isDefined) {
          val url = t.url.get
          s"""<a href="$baseurl/$url">${t.title}</a>"""
        }
        else /*if (t.subsection.nonEmpty)*/ {
          ctx.docbase.error(s"url was defined for subsection with title: ${t.title}, remove url to get toggleable entries")
          t.title
        }
    }

    override def render(ctx: TemplateContext, nodes: LNode*): AnyRef =
      (nodes(0).render(ctx), nodes(1).render(ctx)) match {
        case (t: Title, parent: String) => renderTitle(t, parent)
        case (t: Title, _) => renderTitle(t, "./") // file is in top dir
        case _ => null
      }
  }

  /** Allows the extraction of docstrings from the given path. E.g:
    *
    * ```html
    * {% docstring "scala.collection.Seq" %}
    * ```
    *
    * In Scaladoc, objects are denoted by a name ending in '$'. This means that
    * a path that goes through, or targets an object need to appropriately
    * intersperse these, e.g:
    *
    * ```html
    * {% docstring "scala.collection.Seq$" %}
    * ```
    */
  case class Docstring(params: Map[String, AnyRef]) extends Tag("docstring") {
    private def find(xs: List[String], ent: Entity with Members): Option[Entity] = xs match {
      case Nil => None
      case x :: Nil =>
        ent.members collect { case e: Entity with Members => e } find (_.path.last == x)
      case x :: xs =>
        ent.members collect { case e: Entity with Members => e } find (_.path.last == x) flatMap (find(xs, _))
    }

    override def render(ctx: TemplateContext, nodes: LNode*): AnyRef = nodes(0).render(ctx) match {
      case query: String =>
        params.get("originalDocs").collect {
          case docs: Map[String, Package] @unchecked =>
            val search = query.split("\\.")
            if (search.isEmpty || !docs.contains(search.head)) null
            else find(search.tail.toList, docs(search.head)).flatMap(_.comment.map(_.body)).getOrElse(null)
        }.getOrElse(null)
      case _ => null
    }
  }
}
