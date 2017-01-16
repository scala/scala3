package dotty.tools
package dottydoc
package staticsite

import model.references._
import java.util.{ Map => JMap }

import liqp.filters.Filter

/** Custom liquid template filters */
object filters {

  /** Used to reverse arrays:
    *
    * ```html
    * {% assign array = "1,2,3,4,5" | split: "," %}
    * {{ array | reverse }}
    * ```
    */
  final class Reverse extends Filter("reverse") {
    override def apply(value: Any, params: AnyRef*): AnyRef = {
      val array = super.asArray(value)
      if (array.length == 0) null
      else array.reverse
    }
  }

  /** Renders a `Reference` as HTML. Example:
    *
    * ```html
    * {{ ref | renderRef }}
    * ```
    *
    * where `ref` is:
    *
    * ```scala
    * TypeReference("Seq", MaterializedLink("Seq", "../../scala/collection/Seq.html"), Nil)
    * ```
    *
    * will render:
    *
    * ```html
    * <a href="../../scala/collection/Seq.html">Seq</a>
    * <span class="no-left no-right">[</span>
    * A
    * <span class="no-left">]</span>
    * ```
    */
  final class RenderReference extends Filter("renderRef") {
    // might need to be rewritten to be stack safe
    private def renderReference(ref: Reference): String = ref match {
      case TypeReference(_, tpeLink, paramLinks) => {
        if (paramLinks.nonEmpty) {
          s"""|${renderLink(tpeLink)}
              |<span class="no-left no-right">[</span>
              |${ paramLinks.map(renderReference).mkString("""<span class="">, </span>""") }
              |<span class="no-left">]</span>""".stripMargin
        }
        else renderLink(tpeLink)
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

    override def apply(value: Any, params: AnyRef*): AnyRef = value match {
      case value: JMap[String, _] @unchecked =>
        renderReference(value.get("scala").asInstanceOf[Reference])
      case _ =>
        /*dottydoc.*/println(s"couldn't render: '$value'")
        null
    }
  }

  /** Renders a `MaterializableLink` into a HTML anchor tag. If the link is
    * `NoLink` it will just return a string with the link's title.
    */
  final class RenderLink extends Filter("renderLink") {
    override def apply(value: Any, params: AnyRef*): AnyRef = value match {
      case value: JMap[String, _] @unchecked =>
        renderLink(value.get("scala").asInstanceOf[MaterializableLink])
      case _ =>
        /*dottydoc.*/println(s"couldn't render: '$value'")
        null
    }
  }

  private[this] def renderLink(link: MaterializableLink): String = link match {
    case MaterializedLink(title, target) =>
      s"""<a href="$target">$title</a>"""
    case _ => link.title
  }
}
